(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of Hisoka                                                                *)
(* Copyright (C) 2024 Yves Ndiaye                                                             *)
(*                                                                                            *)
(* Hisoka is free software: you can redistribute it and/or modify it under the terms          *)
(* of the GNU General Public License as published by the Free Software Foundation,            *)
(* either version 3 of the License, or (at your option) any later version.                    *)
(*                                                                                            *)
(* Hisoka is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;        *)
(* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           *)
(* PURPOSE.  See the GNU General Public License for more details.                             *)
(* You should have received a copy of the GNU General Public License along with Hisoka.       *)
(* If not, see <http://www.gnu.org/licenses/>.                                                *)
(*                                                                                            *)
(**********************************************************************************************)

open Cmdliner
open Libhisoka
open Util
module StringSet = Util.Strategy.StringSet

let name = "decrypt"

type cmd_decrypt = {
  groups : string list;
  strategy : Strategy.strategy_group;
  out_dir : string option;
  files : string list;
}

type t = cmd_decrypt

let files_term =
  Arg.(
    value & pos_all string []
    & info [] ~docv:"FILES"
        ~doc:
          "Files to decrypt. If no file is provided, all the files which are \
           matched by the groups and strategy will be decrypted"
  )

let out_dir_term =
  Arg.(
    value
    & opt (some dir) None
    & info ~docv:"DIRECTORY" ~absent:"current directory"
        ~doc:"Write decrypted files in the given $(b,DIRECTORY)" [ "out-dir" ]
  )

let cmd_term run =
  let combine groups strategy files out_dir =
    run @@ { groups; strategy; files; out_dir }
  in
  Term.(
    const combine
    $ Common.groups_term ~docv:"GROUP"
        ~doc:"Decrypt all files belonging to GROUP"
    $ Common.stragtegy_group_term $ files_term $ out_dir_term
  )

let cmd_doc = "Decrypt encrypted files"

let cmd_man =
  [
    `S Manpage.s_description;
    `P "Decrypt encrypted files";
    `S Manpage.s_examples;
    `I ("Decrypt the file $(b,file.txt)", "$(iname) file.txt");
    `I ("Decrypt all files", "$(iname)");
    `I
      ( "Decrypt all files which are in the groups $(b,g1) and $(b,g2)",
        "$(iname) -s all -g g1 -g g2"
      );
    `I
      ( "Decrypt all files which are in the groups $(b,g1) or $(b,g2)",
        "$(iname) -s any -g g1 -g g2"
      );
  ]

let cmd run =
  let info = Cmd.info name ~doc:cmd_doc ~man:cmd_man in
  Cmd.v info (cmd_term run)

let run decrypt_cmd =
  let () = App.App.check_app_initialized () in
  let { groups; strategy; files; out_dir } = decrypt_cmd in
  let encrypted_key =
    Input.ask_password_encrypted ~prompt:"Enter the master password : " ()
  in
  let manager = Manager.Manager.decrypt ~key:encrypted_key () in
  let manager_filtered =
    Manager.Manager.fetch_group_files ~strategy ~groups ~files manager
  in
  match Manager.Manager.is_empty manager_filtered with
  | false ->
      let outdir = Option.value ~default:(Sys.getcwd ()) out_dir in
      let () =
        Manager.Manager.decrypt_files ~dir_path:outdir ~key:encrypted_key
          manager_filtered ()
      in
      ()
  | true ->
      raise Error.(HisokaError No_file_to_decrypt)

let command = cmd run
