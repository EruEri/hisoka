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

let name = "list"

type cmd_list = { strategy : Strategy.strategy_group; groups : string list }
type t = cmd_list

let cmd_term run =
  let combine strategy groups = run @@ { strategy; groups } in
  Term.(
    const combine $ Common.stragtegy_group_term
    $ Common.groups_term ~docv:"FILES" ~doc:"Files to list"
  )

let cmd_doc = "List of encrypted files"

let cmd_man =
  [
    `S Manpage.s_description;
    `P "List of encrypted files";
    `S Manpage.s_examples;
    `I ("List the file $(b,file.txt)", "$(iname) file.txt");
    `I ("List all files", "$(iname)");
    `I
      ( "List all files which are in the groups $(b,g1) and $(b,g2)",
        "$(iname) -s all -g g1 -g g2"
      );
    `I
      ( "List all files which are in the groups $(b,g1) or $(b,g2)",
        "$(iname) -s any -g g1 -g g2"
      );
  ]

let cmd run =
  let info = Cmd.info name ~doc:cmd_doc ~man:cmd_man in
  Cmd.v info (cmd_term run)

let run cmd_list =
  let module StringSet = Util.Strategy.StringSet in
  let () = App.App.check_app_initialized () in
  let { strategy; groups } = cmd_list in
  let encrypted_key =
    Input.ask_password_encrypted ~prompt:"Enter the master password : " ()
  in
  let manager = Manager.Manager.decrypt ~key:encrypted_key () in
  let items_list = Manager.Manager.list_info manager in
  let items_list =
    match groups with
    | [] ->
        items_list
    | groups ->
        let filter_groups = StringSet.of_list groups in
        items_list
        |> List.filter (fun item ->
               let open Items.Info in
               let igroups = StringSet.of_list item.groups in
               Strategy.fstrategy strategy filter_groups igroups
           )
  in
  let () =
    items_list
    |> List.iter (fun info ->
           let open Items.Info in
           Printf.printf "name : %s, group : [%s]\n" info.name
             (String.concat ", " info.groups)
       )
  in
  ()

let command = cmd run