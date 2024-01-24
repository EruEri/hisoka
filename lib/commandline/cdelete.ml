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

let name = "delete"

type cmd_delete = {
  groups : string list;
  strategy : Strategy.strategy_group;
  files : string list;
}

type t = cmd_delete

let files_term =
  Arg.(
    value & pos_all string [] & info [] ~docv:"FILES" ~doc:"Files to delete"
  )

let cmd_term run =
  let combine groups strategy files = run @@ { groups; strategy; files } in
  Term.(
    const combine
    $ Common.groups_term ~docv:"GROUP"
        ~doc:
          "Delete files belonging to GROUP. If no$(b, FILES) is provided, \
           $(opt) will delete all$(b, FILES) in $(docv)"
    $ Common.stragtegy_group_term $ files_term
  )

let cmd_doc = "Delete files from hisoka"

let cmd_man =
  [
    `S Manpage.s_description;
    `P "Delete files";
    `S Manpage.s_examples;
    `I ("Delete the file $(b,file.txt)", "$(iname) file.txt");
    `I ("Delete all files", "$(iname)");
    `I
      ( "Delete all files which are in the groups $(b,g1) and $(b,g2)",
        "$(iname) -s all -g g1 -g g2"
      );
    `I
      ( "Delete all files which are in the groups $(b,g1) or $(b,g2)",
        "$(iname) -s any -g g1 -g g2"
      );
  ]

let cmd run =
  let info = Cmd.info name ~doc:cmd_doc ~man:cmd_man in
  Cmd.v info (cmd_term run)

let run delete_cmd =
  let () = App.App.check_app_initialized () in
  let { groups; strategy; files } = delete_cmd in
  let delete_all_in_group = groups <> [] && delete_cmd.files = [] in
  let string_groups = String.concat ", " groups in
  let s = match groups with [] | _ :: [] -> "" | _ -> "s" in
  let prompt =
    match strategy with
    | Strategy.All ->
        Printf.sprintf
          "Are you sure about deleting all files contains [%s] as group%s"
          string_groups s
    | Any ->
        Printf.sprintf
          "Are you sure about deleting all files contains at least one of \
           the follwing group%s: [%s]"
          s string_groups
    | Exact ->
        Printf.sprintf
          "Are you sure about deleting all files with exacttly the follwing \
           group%s: [%s]"
          s string_groups
  in
  let () =
    if delete_all_in_group then
      let continue =
        Input.confirm_choice
          ~continue_on_wrong_input:(Input.Continue (Some "Wrong input"))
          ~case_insensible:true ~yes:'y' ~no:'n' ~prompt ()
      in
      if not continue then
        raise Error.(HisokaError No_Option_choosen)
  in
  let encrypted_key =
    Input.ask_password_encrypted ~prompt:"Enter the master password : " ()
  in
  let manager = Manager.Manager.decrypt ~key:encrypted_key () in
  let filtered_manager, deleted_files_info =
    Manager.Manager.remove ~strategy ~groups files manager
  in
  match deleted_files_info with
  | [] ->
      print_endline "No files to delete"
  | deleted_files_info ->
      let string_of_files =
        let open Items.Info in
        deleted_files_info
        |> List.map (fun item -> item.Items.External.info.name)
        |> String.concat ", "
      in
      let deleting_file_format =
        Printf.sprintf "Following files will be deleted : %s " string_of_files
      in
      let confirmed_deletion =
        Input.confirm_choice
          ~continue_on_wrong_input:(Input.Continue (Some "Wrong input"))
          ~case_insensible:true ~yes:'y' ~no:'n' ~prompt:deleting_file_format
          ()
      in
      if confirmed_deletion then
        let () =
          deleted_files_info
          |> List.iter (fun file_info ->
                 let pathbuf =
                   Path.push file_info.Items.External.encrypted_file_name
                     App.AppLocation.hisoka_data_dir
                 in
                 let path = Path.to_string pathbuf in
                 Util.FileSys.rmrf path ()
             )
        in
        let () =
          Manager.Manager.encrypt ~max_iter:3 ~raise_on_conflicts:true
            ~key:encrypted_key filtered_manager ()
        in
        Printf.printf "Deleted : %s\n" string_of_files
      else
        raise Error.(HisokaError No_Option_choosen)

let command = cmd run