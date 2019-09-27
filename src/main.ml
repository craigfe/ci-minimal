open Current.Syntax
module Git = Current_git
module Docker = Current_docker.Default

let () = Logging.init ()

(* Dockerfiles *)
let dockerfile_project ~base ~ocaml_version =
  let open Dockerfile in
  from (Docker.Image.hash base)
  @@ run "opam switch %s" ocaml_version
  @@ workdir "/src"
  @@ add ~src:[ "*.opam" ] ~dst:"/src/" ()
  @@ env [ ("OPAMERRLOGLEN", "0") ]
  @@ run
       "opam install . --show-actions --deps-only -t | awk '/- install/{print \
        $3}' | xargs opam depext -iy"
  @@ copy ~src:[ "." ] ~dst:"/src/" ()

let dockerfile_format ~base ~ocaml_version ~ocamlformat_version =
  let open Dockerfile in
  from (Docker.Image.hash base)
  @@ user "opam" @@ workdir "/home/opam/src"
  @@ run "opam switch %s" ocaml_version
  (* @@ run "git -C /home/opam/opam-repository pull" *)
  @@ run "opam depext ocamlformat=%s" ocamlformat_version
  @@ run "opam install ocamlformat=%s" ocamlformat_version

(* Pipeline stages *)
let build ~src ~ocaml_version base =
  let dockerfile =
    let+ base = base in
    dockerfile_project ~base ~ocaml_version
  in
  Docker.build ~label:"dependencies" ~pull:true ~dockerfile (`Git src)

let test (img : Docker.Image.t Current.t) =
  Docker.run img ~args:[ "opam"; "install"; "-tv"; "." ]

let format (base : Docker.Image.t Current.t) ~ocamlformat_version
    ~ocaml_version =
  let dockerfile =
    let+ base = base in
    dockerfile_format ~base ~ocaml_version ~ocamlformat_version
  in
  let img = Docker.build ~label:"lint" ~pull:false ~dockerfile `No_context in
  Docker.run img ~args:[ "dune"; "build"; "@fmt" ]

(* Main pipeline*)
let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

let pipeline ~repo () =
  let src = Git.Local.head_commit repo in
  let opam_base = Docker.pull ~schedule:weekly "ocaml/opam2" in
  let img = build ~src ~ocaml_version:"4.08" opam_base in
  Current.all
    [
      test img;
      format img ~ocaml_version:"4.08" ~ocamlformat_version:"0.11.0";
    ]

let main config mode repo =
  let repo = Git.Local.v (Fpath.v repo) in
  let engine = Current.Engine.create ~config (pipeline ~repo) in
  Logging.run
    (Lwt.choose [ Current.Engine.thread engine; Current_web.run ~mode engine ])

(* Command-line parsing *)

open Cmdliner

let repo =
  Arg.value
  @@ Arg.pos 0 Arg.dir (Sys.getcwd ())
  @@ Arg.info ~doc:"The directory containing the .git subdirectory."
       ~docv:"DIR" []

let cmd =
  let doc = "Build the head commit of a local Git repository using Docker." in
  ( Term.(const main $ Current.Config.cmdliner $ Current_web.cmdliner $ repo),
    Term.info "build_matrix" ~doc )

let () = Term.(exit @@ eval cmd)
