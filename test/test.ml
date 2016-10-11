open! Core.Std
open! Async.Std
open! Async_http

let () =
  let f = (Writer.to_formatter (Lazy.force Writer.stdout)) in
  Logs.set_reporter (Logs.format_reporter ~dst:f ~app:f ());
  Logs.Src.set_level Async_http_log.src (Some Logs.Debug)
(* Logs.Src.set_level Async_http_log.Res.src (Some Logs.Debug) *)

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

let print_f_response r =
  Result.ok_exn r
  |> format_f_response
  |> print_endline

let print_json_response r =
  let ({Response.body} as r') = Result.ok_exn r in
  {r' with Response.body = Yojson.Basic.pretty_to_string body }
  |> format_response
  |> print_endline

let%expect_test "basic get request" =
  request "http://httpbin.org/get"
  |> header "X-Test-Header" "some-value"
  |> query_param "some-param" "other&&value"
  |> get
  >>| print_f_response >>= fun () ->
  [%expect {|
    test.native: [INFO] Make HTTP request (Inet (httpbin.org 80))
    test.native: [DEBUG] HTTP checkout conn for (Inet (httpbin.org 80))
    test.native: [DEBUG] HTTP create new conn for (Inet (httpbin.org 80))
    test.native: [DEBUG] HTTP Raw: GET /get?some-param=other%26%26value HTTP/1.1
    test.native: [DEBUG] HTTP Raw: X-Test-Header: some-value
    test.native: [DEBUG] HTTP Raw: Host: httpbin.org
    test.native: [DEBUG] HTTP checkin conn for (Inet (httpbin.org 80))
    Status: 200
    Headers: ((Server nginx) (Date ".+") (regexp)
     (Content-Type application/json) (Content-Length [0-9]+) (Connection keep-alive) (regexp)
     (Access-Control-Allow-Origin *) (Access-Control-Allow-Credentials true))
    Body:
    {
      "args": {
        "some-param": "other&&value"
      },
      "headers": {
        "Host": "httpbin.org",
        "X-Test-Header": "some-value"
      },
      "origin": ".+", (regexp)
      "url": "http://httpbin.org/get?some-param=other%26%26value"
    } |}]

let%expect_test "basic post request" =
  request "http://httpbin.org/post"
  |> body "hello world"
  |> post
  >>| print_f_response >>= fun () ->
  [%expect {|
    test.native: [INFO] Make HTTP request (Inet (httpbin.org 80))
    test.native: [DEBUG] HTTP checkout conn for (Inet (httpbin.org 80))
    test.native: [DEBUG] HTTP Raw: POST /post HTTP/1.1
    test.native: [DEBUG] HTTP Raw: Host: httpbin.org
    test.native: [DEBUG] HTTP Raw: Content-Length: 11
    test.native: [DEBUG] HTTP checkin conn for (Inet (httpbin.org 80))
    Status: 200
    Headers: ((Server nginx) (Date ".+") (regexp)
     (Content-Type application/json) (Content-Length [0-9]+) (Connection keep-alive) (regexp)
     (Access-Control-Allow-Origin *) (Access-Control-Allow-Credentials true))
    Body:
    {
      "args": {},
      "data": "hello world",
      "files": {},
      "form": {},
      "headers": {
        "Content-Length": "11",
        "Host": "httpbin.org"
      },
      "json": null,
      "origin": ".+", (regexp)
      "url": "http://httpbin.org/post"
    } |}]


let%expect_test "parsed body" =
  request "http://httpbin.org/get" |> parser Yojson.Basic.from_string |> get
  >>| print_json_response >>= fun () ->
  [%expect {|
    test.native: [INFO] Make HTTP request (Inet (httpbin.org 80))
    test.native: [DEBUG] HTTP checkout conn for (Inet (httpbin.org 80))
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

let%expect_test "chunked encoding" =
  request "https://httpbin.org/stream/1" |> get
  >>| print_response >>= fun () ->
  [%expect {|
    test.native: [INFO] Make HTTP request (Inet (httpbin.org 443))
    test.native: [DEBUG] HTTP checkout conn for (Inet (httpbin.org 443))
    test.native: [DEBUG] HTTP create new conn for (Inet (httpbin.org 443))
    test.native: [DEBUG] HTTP Raw: GET /stream/1 HTTP/1.1
    test.native: [DEBUG] HTTP Raw: Host: httpbin.org
    test.native: [DEBUG] HTTP checkin conn for (Inet (httpbin.org 443))
    {"url": "https://httpbin.org/stream/1", "headers": {"Host": "httpbin.org"}, "args": {}, "id": 0, "origin": ".+"} (regexp) |}]

let () =
  Ppx_inline_test_lib.Runtime.exit ()

(* let _ = *)
(*   request "https://httpbin.org/stream/1" |> get >>| print_json_response |> don't_wait_for; *)
(*   Scheduler.go () *)
