module C = Configurator.V1

let chafa_cflag = "cflags_chafa.sexp"
let chafa_clib = "clibrary_flags_chafa.sexp"

let ncursesw_cflag = "cflags_ncursesw.sexp"
let ncursesw_clib = "clibrary_flags_ncursesw.sexp"



let pkg_config ~cflags ~clibs ~libname () = 
  C.main ~name:libname (fun c ->
    let conf =
      match C.Pkg_config.get c with
      | None -> C.die "pkg-config not found"
      | Some pc ->
        match (C.Pkg_config.query pc ~package:libname) with
        | None -> C.die "%s pkg-config query not found" libname
        | Some deps -> deps
    in

    (* let () = Printf.eprintf "Pkg out = [%s]\n" (String.concat ", " conf.cflags) in  *)
    C.Flags.write_sexp cflags conf.cflags;
    C.Flags.write_sexp clibs conf.libs
  )

let () = pkg_config ~cflags:chafa_cflag ~clibs:chafa_clib ~libname:"chafa" ()
let () = pkg_config ~cflags:ncursesw_cflag ~clibs:ncursesw_clib ~libname:"ncursesw" ()
(* let () = pkg_config ~libname:"ncursesw" () *)
