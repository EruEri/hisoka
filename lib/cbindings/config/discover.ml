module C = Configurator.V1

let outcflag = "c_flags.sexp"
let out_clib = "c_library_flags.sexp"

let pkg_config ~libname () = 
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
    C.Flags.write_sexp outcflag conf.cflags;
    C.Flags.write_sexp out_clib conf.libs
  )

let () = pkg_config ~libname:"chafa" ()
(* let () = pkg_config ~libname:"ncursesw" () *)
