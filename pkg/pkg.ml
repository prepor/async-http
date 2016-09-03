#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "async_http" ~build:(Pkg.build ~cmd:(fun c os files -> OS.Cmd.run @@ Cmd.(Pkg.build_cmd c os % "-plugin-tag" % "package(ppx_driver.ocamlbuild)" %% of_list files)) ())
    @@ fun c ->
  Ok [ Pkg.mllib "src/async_http.mllib";
       Pkg.test ~args:Cmd.(v "inline-test-runner" % "async_http") "test/test"; ]
