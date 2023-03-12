module C = Configurator.V1

let cflag = Printf.sprintf "cflags_%s.sexp"
let clibs = Printf.sprintf "clibrary_flags_%s.sexp"

let generate_flag_libs name = 
  cflag name, clibs name


let chafa_cflag, chafa_clib = generate_flag_libs "chafa"

(* let ncursesw_cflag, ncursesw_clib = generate_flag_libs "ncursesw" *)

let magicwand_cflag, magicwand_clib = generate_flag_libs "MagickWand"




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
let () = pkg_config ~cflags:magicwand_cflag ~clibs:magicwand_clib ~libname:"MagickWand" ()
(* let () = pkg_config ~libname:"ncursesw" () *)
