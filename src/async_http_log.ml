let src = Logs.Src.create ~doc:"HTTP Client for Async" "http"

include (val Logs.src_log src : Logs.LOG)

module Res = struct
  let src = Logs.Src.create ~doc:"HTTP Client for Async. Response logs" "http-response"
  include (val Logs.src_log src : Logs.LOG)
end
