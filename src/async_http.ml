open! Core.Std
open! Async.Std

module L = Async_http_log

module Response = struct
  type _ body = String : string body
              | Parsed : (string -> 'a) -> 'a body

  type 'a t = { status : int;
                version : string;
                headers : (string, string) List.Assoc.t;
                body : 'a}
end

type addr = [`Unix of string | `Inet of (string * int)] [@@deriving sexp]

exception ProtocolError of string
exception FailedRequest of string
exception AddrError of string * string

module Protocol = struct
  open Angstrom

  module P = struct
    let is_space =
      function | ' ' | '\t' -> true | _ -> false

    let is_eol =
      function | '\r' | '\n' -> true | _ -> false

    let is_hex =
      function | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true | _ -> false

    let is_digit =
      function '0' .. '9' -> true | _ -> false

    let is_separator =
      function
      | ')' | '(' | '<' | '>' | '@' | ',' | ';' | ':' | '\\' | '"'
      | '/' | '[' | ']' | '?' | '=' | '{' | '}' | ' ' | '\t' -> true
      | _ -> false

    let is_token =
      (* The commented-out ' ' and '\t' are not necessary because of the range at
       * the top of the match. *)
      function
      | '\000' .. '\031' | '\127'
      | ')' | '(' | '<' | '>' | '@' | ',' | ';' | ':' | '\\' | '"'
      | '/' | '[' | ']' | '?' | '=' | '{' | '}' (* | ' ' | '\t' *) -> false
      | _ -> true
  end

  let token = take_while1 P.is_token
  let digits = take_while1 P.is_digit
  let spaces = skip_while P.is_space

  let lex p = p <* spaces

  let version =
    string "HTTP/" *>
    lift2 (fun major minor -> (major ^ "." ^ minor))
      (digits <* char '.')
      digits

  let uri =
    take_till P.is_space

  let meth = token
  let eol = string "\r\n"

  let request_first_line =
    lift3 (fun meth uri version -> (meth, uri, version))
      (lex meth)
      (lex uri)
      version

  let response_first_line =
    lift3 (fun version status msg -> (version, (int_of_string status), msg))
      (lex version)
      (lex (take_till P.is_space))
      (take_till P.is_eol)

  let header =
    let colon = char ':' <* spaces in
    lift2 (fun key value -> (key, value))
      token
      (colon *> take_till P.is_eol)

  let request =
    lift2 (fun (meth, uri, version) headers -> (meth, uri, version, headers))
      (request_first_line   <* eol)
      (many (header <* eol) <* eol)

  let response_header =
    lift2 (fun (version, status, msg) headers ->
        L.Res.debug (fun m -> m "HTTP Raw Response: HTTP/%s %i %s" version status msg);
        List.iter headers ~f:(fun (k, v) ->
            L.Res.debug (fun m -> m "HTTP Raw Response: %s: %s" k v));
        (version, status, msg, headers))
      (response_first_line  <* eol)
      (many (header <* eol) <* eol)

  let response_body len =
    take len

  let chunk_extension =
    char ';' *> lift2 (fun ext_name ext_val -> (ext_name, ext_val))
      (take_while P.is_token)
      (* FIXME or quoted-string *)
      (option None (char '=' *> take_while P.is_token >>| Option.some))

  let chunk =
    take_while1 P.is_hex >>= (fun len ->
        let len' = (int_of_string ("0x" ^ len)) in
        let header = (skip_many chunk_extension) *> eol in
        if len' > 0 then header *> take (int_of_string ("0x" ^ len)) <* eol
        else header *> return "")

  let trailer =
    skip_many (header <* eol)

  let response_chunked =
    many chunk <* trailer <* eol >>| String.concat

  let is_success success_if status =
    match success_if with
    | `Always -> true
    | `List codes -> List.mem codes status
    | `Range (from, to_) -> from <= status && status < to_
    | `Code_2xx -> 200 <= status && status < 300

  let response (type r) success_if (typ : r Response.body) =
    response_header >>= fun (version, status, msg, headers) ->
    let make_resp_body (body : string) : r =
      L.Res.debug (fun m -> m "HTTP Raw Response: %s" body);
      match typ with
      | Response.String-> body
      | Response.Parsed v -> (v body) in
    let content_len = List.Assoc.find headers ~equal:String.Caseless.equal "Content-Length" in
    let transfer_encoding = List.Assoc.find headers ~equal:String.Caseless.equal "Transfer-Encoding" in
    let body_parser = match (content_len, transfer_encoding) with
    | (Some len, _) -> (response_body (int_of_string len))
    | (_, Some "chunked") -> response_chunked
    | _ -> string "" in
    lift (fun body ->
        if (is_success success_if status) then
          Ok {Response.status; version; headers; body = make_resp_body body}
        else Error (status, body))
      body_parser

end

type meth = Get | Post | Put | Delete | Head | Options | Patch

let meth_to_string = function
| Get -> "GET"
| Post -> "POST"
| Put -> "PUT"
| Delete -> "DELETE"
| Head -> "HEAD"
| Options -> "OPTIONS"
| Patch -> "PATCH"

module Blueprint = struct
  type ('a, 'b) t = { addr : (addr, exn) Result.t;
                      is_ssl : bool;
                      headers : (string * string) list;
                      uri : Uri.t;
                      body : string option;
                      is_persistent : bool;
                      response_type : 'b Response.body;
                      success_if : [`Always | `List of int list | `Range of (int * int) | `Code_2xx]}
    constraint 'a = [< `With_body | `Without_body ]
end

let ssl_connect net_to_ssl ssl_to_net =
  let open Async_ssl.Std in
  let net_to_ssl = Reader.pipe net_to_ssl in
  let ssl_to_net = Writer.pipe ssl_to_net in
  let app_to_ssl, app_wr = Pipe.create () in
  let app_rd, ssl_to_app = Pipe.create () in
  Ssl.client ~app_to_ssl ~ssl_to_app ~net_to_ssl ~ssl_to_net ()
  |> Deferred.Or_error.ok_exn
  >>= fun conn ->
  Reader.of_pipe (Info.of_string "async_ssl_reader") app_rd >>= fun app_rd ->
  Writer.of_pipe (Info.of_string "async_ssl_writer") app_wr >>| fun (app_wr,_) ->
  (app_rd, app_wr)

type connection_spec = (addr * bool) [@@deriving sexp]

let make_socket (addr, is_ssl) =
  let%bind fd = match addr with
  | `Inet (host, port) ->
      let%bind addr' = Unix.Inet_addr.of_string_or_getbyname host in
      let%map s = Socket.connect (Socket.create Socket.Type.tcp) (`Inet (addr', port)) in
      Socket.fd s
  | `Unix addr ->
      let%map s = Socket.connect (Socket.create Socket.Type.unix) (`Unix addr) in
      Socket.fd s in
  let w = Writer.create fd in
  let r = (Reader.create fd) in
  let%map (r', w') = match is_ssl with
  | true -> ssl_connect r w
  | false -> return (r, w) in
  (r', w', fd)


module PoolConn = struct
  module P = struct
    type t = connection_spec
    let t_of_sexp = connection_spec_of_sexp
    let sexp_of_t = sexp_of_connection_spec
    let compare = compare
    let hash = Hashtbl.hash
  end

  type p = P.t
  type t = (Reader.t * Writer.t * Fd.t)

  let create = make_socket
  let is_valid (_, _, fd) = not @@ Fd.is_closed fd
  let destroy (_, _, fd) = Fd.close fd |> don't_wait_for
end

module Pool = Async_http_pool.Make(PoolConn)

let pool = Pool.create ()

let request_of_addr' addr =
  let host_header = match addr with
  | Ok `Inet (host, 80) | Ok `Inet (host, 443) -> host
  | Ok `Inet (host, port) -> (sprintf "%s:%i" host port)
  | _ -> "" in
  {Blueprint.addr;
   is_ssl = false; headers = [("Host", host_header)]; uri = Uri.empty; body = None;
   is_persistent = true; response_type = Response.String;
   success_if = `Code_2xx}

let request_of_addr addr =
  request_of_addr' (Ok addr)

let handle_request bp meth (r,w) =
  let open Blueprint in
  Writer.set_raise_when_consumer_leaves w true;
  let module W = Writer in
  let raw = (meth_to_string meth) ^ " " ^ (Uri.path_and_query bp.uri) ^ " HTTP/1.1" in
  L.debug (fun m -> m "HTTP Raw: %s" raw);
  W.write w (raw ^ "\r\n");
  List.iter bp.headers ~f:(fun (k,v) ->
      let raw = k ^ ": " ^ v in
      L.debug (fun m -> m "HTTP Raw: %s" raw);
      W.write w (raw ^ "\r\n"));
  (match bp.body with
  | Some s ->
      let raw = "Content-Length: " ^ Int.to_string (String.length s) in
      L.debug (fun m -> m "HTTP Raw: %s" raw);
      W.write w (raw ^ "\r\n");
      W.write w "\r\n";
      W.write w s;
  | None ->
      W.write w "\r\n");

  W.flushed w >>= fun () ->
  match%map Angstrom_async.parse (Protocol.response bp.success_if bp.response_type) r with
  | Ok Ok v -> v
  | Ok Error (status, v) -> raise (FailedRequest (sprintf "status %i, response: %s" status v))
  | Error v -> raise (ProtocolError v)

let persistent_request bp connection_spec meth =
  let%bind (r, w, fd) as obj = Pool.checkout pool connection_spec in
  match%map try_with ~extract_exn:true (fun () -> handle_request bp meth (r,w)) with
  | Ok res ->
      (match List.Assoc.find res.Response.headers ~equal:String.Caseless.equal "Connection" with
      | Some "close" -> (Fd.close fd |> don't_wait_for; res)
      | _ -> (Pool.checkin pool connection_spec obj; res))
  | Error exn -> (Fd.close fd |> don't_wait_for; raise exn)

let one_shot_request bp addr meth =
  let%bind (r,w,fd) = make_socket addr in
  let%bind res = handle_request bp meth (r,w) in
  let%map () = Fd.close fd in
  res

let make_request' bp meth =
  let open Blueprint in
  let addr = Result.ok_exn bp.addr in
  if bp.is_persistent then persistent_request bp (addr, bp.is_ssl) meth
  else one_shot_request bp (addr, bp.is_ssl) meth

let format_bp bp =
  let open Blueprint in
  (  match bp.addr with
  | Ok v -> (sexp_of_addr v)
  | Error (AddrError (orig, err)) -> Sexp.(List [Atom "BadAddr"; Atom orig; Atom err])
  | Error e -> Sexp.(List [Atom "UnexpectedAddrError"]))
  |> Sexp.to_string_hum

let make_request meth bp =
  L.info (fun m -> m "Make HTTP request %s" (format_bp bp));
  try_with ~extract_exn:true (fun () -> make_request' bp meth)
    ~name:(sprintf "HTTP request: %s" (format_bp bp))

let header name value bp =
  Blueprint.{bp with headers = (name, value)::bp.headers}

let headers pairs bp =
  Blueprint.{bp with headers = pairs @ bp.headers}

let path p bp =
  Blueprint.{ bp with uri = Uri.with_path bp.uri p }

let body s bp =
  { bp with Blueprint.body = Some s }

let query_param name value bp =
  Blueprint.{ bp with uri = Uri.add_query_param' bp.uri (name, value) }

let query_params pairs bp =
  Blueprint.{ bp with uri = Uri.add_query_params' bp.uri pairs }

let ssl bp = { bp with Blueprint.is_ssl = true }

let not_persistent bp = { bp with Blueprint.is_persistent = false }

let parser p bp = { bp with Blueprint.response_type = Response.Parsed p }

let success_if v bp = { bp with Blueprint.success_if = v }

let request_of_uri uri =
  let addr = let open Result.Let_syntax in
    let%bind port = match (Uri.scheme uri, Uri.port uri) with
    | _, Some port -> Ok port
    | Some "http", _ -> Ok 80
    | Some "https", _ -> Ok 443
    | None, None -> Ok 80
    | Some _, None -> Error "Unknown port for this uri" in
    let%map host = (Uri.host uri) |> Result.of_option ~error: "Unknown host" in
    `Inet (host, port) in
  addr
  |> Result.map_error ~f:(fun e -> AddrError (Uri.to_string uri, e))
  |> request_of_addr'
  |> path (Uri.path uri)
  |> (fun bp -> if Some "https" = (Uri.scheme uri) then ssl bp else bp)
  |> fun bp -> List.fold (Uri.query uri) ~init:bp ~f:(fun bp (name, values) ->
      List.fold values ~init:bp ~f:(fun bp value -> query_param name value bp) )

let request uri =
  Uri.of_string uri |> request_of_uri

let get bp = make_request Get bp
let post bp = make_request Post bp
let put bp = make_request Put bp
let delete bp = make_request Delete bp
let options bp = make_request Options bp
let head bp = make_request Head bp
let patch bp = make_request Patch bp

(* let request ~meth ~addr ~path ~headers = *)
(*   let s = Socket.create addr in *)
(*   s *)

(* let test () = *)
(*   (\* let open Async_http in *\) *)
(*   request_of_addr (`Inet ("ya.ru", 443)) *)
(*   |> ssl *)
(*   |> headers [("Host", "ya.ru"); *)
(*               ("User-Agent", "curl/7.43.0"); *)
(*               ("Accept", "*/*")] *)
(*   |> body "lala" *)
(*   |> get *)
