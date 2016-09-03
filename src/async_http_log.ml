let src = Logs.Src.create ~doc:"HTTP Client for Async" "http"

include (val Logs.src_log src : Logs.LOG)
