open! Core.Std
open! Async.Std
open! Async_http

let () =
  let f = (Writer.to_formatter (Lazy.force Writer.stdout)) in
  Logs.set_reporter (Logs.format_reporter ~dst:f ~app:f ());
  Logs.Src.set_level Async_http_log.src (Some Logs.Debug)

let format_f_response {Response.body; status; headers} =
  sprintf "Status: %i\nHeaders: %s\nBody:\n%s"
    status
    (List.Assoc.sexp_of_t String.sexp_of_t String.sexp_of_t headers |> Sexp.to_string_hum)
    body

let format_response {Response.body} = body

let print_response r =
  Result.ok_exn r
  |> format_response
  |> print_endline

let print_json_response r =
  let ({Response.body} as r') = Result.ok_exn r in
  {r' with Response.body = Yojson.Basic.pretty_to_string body }
  |> format_response
  |> print_endline

let%expect_test "basic get request" =
  request "http://httpbin.org/get" |> get
  >>| print_response >>= fun () ->
  [%expect {|
    test.native: [INFO] Make HTTP request (Inet (httpbin.org 80))
    test.native: [DEBUG] HTTP checkout conn for (Inet (httpbin.org 80))
    test.native: [DEBUG] HTTP create new conn for (Inet (httpbin.org 80))
    test.native: [DEBUG] HTTP Raw: GET /get HTTP/1.1
    test.native: [DEBUG] HTTP Raw: Host: httpbin.org
    test.native: [DEBUG] HTTP checkin conn for (Inet (httpbin.org 80))
    {
      "args": {},
      "headers": {
        "Host": "httpbin.org"
      },
      "origin": ".+", (regexp)
      "url": "http://httpbin.org/get"
    } |}]

let%expect_test "parsed body" =
  request "http://httpbin.org/get" |> parser Yojson.Basic.from_string |> get
  >>| print_json_response >>= fun () ->
  [%expect {|
    test.native: [INFO] Make HTTP request (Inet (httpbin.org 80))
    test.native: [DEBUG] HTTP checkout conn for (Inet (httpbin.org 80))
    test.native: [DEBUG] HTTP connection for (Inet (httpbin.org 80)) is not valid, forget about it
    test.native: [DEBUG] HTTP create new conn for (Inet (httpbin.org 80))
    test.native: [DEBUG] HTTP Raw: GET /get HTTP/1.1
    test.native: [DEBUG] HTTP Raw: Host: httpbin.org
    test.native: [DEBUG] HTTP checkin conn for (Inet (httpbin.org 80))
    {
      "args": {},
      "headers": { "Host": "httpbin.org" },
      "origin": ".+", (regexp)
      "url": "http://httpbin.org/get"
    } |}]

(* is there better way to expect errors? *)
let print_error res =
  let opt = Result.error res in
  Option.value_exn opt |> Monitor.extract_exn |> Exn.to_string |> print_endline

let%expect_test "bad url" =
  request "/get" |> get >>| print_error >>= fun () ->
  [%expect {|
    test.native: [INFO] Make HTTP request (BadAddr /get "Unknown host")
    ("Async_http.AddrError(\"/get\", \"Unknown host\")") |}]
  >>= fun () -> request "lala://ya.ru/get" |> get >>| print_error >>= fun () ->
  [%expect {|
    test.native: [INFO] Make HTTP request (BadAddr lala://ya.ru/get "Unknown port for this uri")
    ("Async_http.AddrError(\"lala://ya.ru/get\", \"Unknown port for this uri\")") |}]


let () =
  Ppx_inline_test_lib.Runtime.exit ()
