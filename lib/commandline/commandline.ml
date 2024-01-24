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

let version =
  match Build_info.V1.version () with
  | None ->
      "n/a"
  | Some v ->
      Build_info.V1.Version.to_string v

let root_doc = "the encrypted-file manager"

type t = bool

let change_term =
  Arg.(
    value & flag
    & info [ "change-master-password" ] ~doc:"Change the master password"
  )

let cmd_term run =
  let combine change = run @@ change in
  Term.(const combine $ change_term)

let run_base change_password =
  ignore change_password;
  ()

let root_man =
  [
    `S Manpage.s_description;
    `P "hisoka allows you to store any files by encrypting them";
  ]

let root_info = Cmd.info "hisoka" ~doc:root_doc ~man:root_man ~version

let subcommands =
  [
    Cinit.command;
    Cadd.command;
    Clist.command;
    Cdecrypt.command;
    Cdelete.command;
  ]

let parse () = Cmd.group ~default:(cmd_term run_base) root_info subcommands
let eval () = () |> parse |> Cmd.eval