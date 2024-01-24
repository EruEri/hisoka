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

module StringSet = Util.Strategy.StringSet

let name = "add"

type cmd_add = {
  groups : string list;
  existing_groups : string list;
  files : string list;
}

type t = cmd_add

let existing_group_term =
  let einfo =
    Arg.info [ "e"; "existing-group" ] ~docv:"EXISTING_GROUP"
      ~doc:"Append the file to the group $(docv). The group must exist"
  in
  Arg.(value & opt_all string [] & einfo)

let cmd_term run =
  let combine files existing_groups groups =
    run @@ { files; existing_groups; groups }
  in
  Term.(
    const combine
    $ Common.file_term ~docv:"FILES" ~doc:"Files to add to hisoka"
    $ existing_group_term
    $ Common.groups_term ~docv:"GROUP"
        ~doc:"Append the file to the group $(docv)"
  )

let cmd_doc = "Add files to hisoka"

let cmd_man =
  [
    `S Manpage.s_description;
    `P "Add files to hisoka";
    `S Manpage.s_examples;
    `I
      ( "Add the file $(b,file.txt) in an existing group $(b,g1)",
        "$(iname) -e g1 file.txt"
      );
    `I
      ( "Add files $(b,file1.txt), $(b,file2.txt) in groups $(b,g1) and \
         $(b,g2)",
        "$(iname) -g g1 -g g2 file1.txt file2.txt"
      );
    `I
      ( "Add files $(b,file1.txt), $(b,file2.txt) in grou $(b,g1) and \
         existing group $(b,e1)",
        "$(iname) -g g1 -e e1 file1.txt file2.txt"
      );
  ]

let cmd run =
  let info = Cmd.info name ~doc:cmd_doc ~man:cmd_man in
  Cmd.v info (cmd_term run)

let run cmd_add =
  let module StringSet = Set.Make(String) in
  let { groups; existing_groups; files } = cmd_add in
  let encrypted_key =
    Libhisoka.Input.ask_password_encrypted ~prompt:"Enter the master password : " ()
  in
  let manager = Manager.Manager.decrypt ~key:encrypted_key () in

  let managers_groups = StringSet.of_list @@ Manager.Manager.groups manager in
  let futures_groups = StringSet.of_list existing_groups in

  let diff = StringSet.diff futures_groups managers_groups in
  let () =
    match StringSet.is_empty diff with
    | true ->
        ()
    | false ->
        raise
        @@ Error.(hisoka_error @@ Non_existing_group (StringSet.elements diff))
  in

  let groups =
    groups
    |> List.rev_append existing_groups
    |> StringSet.of_list |> StringSet.elements
  in
  let manager =
    files
    |> List.fold_left
         (fun manager file ->
           let name, extension =
             (Filename.basename file, Filename.extension file)
           in
           Manager.Manager.add_item_from_file ~groups ~name ~extension
             ~file_name:file manager
         )
         manager
  in
  let () =
    Manager.Manager.encrypt ~key:encrypted_key ~max_iter:3
      ~raise_on_conflicts:true manager ()
  in

  let () =
    List.iter
      (fun f ->
        Printf.printf "File: \"%s\" in groups [%s] Added\n" f
          (String.concat ", " groups)
      )
      files
  in
  ()

let command = cmd run