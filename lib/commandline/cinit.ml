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

let name = "init"

type cmd_init = { force : bool }
type t = cmd_init

let force_term =
  let info = Arg.info [ "f"; "force" ] ~doc:"force the initialization" in
  Arg.value (Arg.flag info)

let cmd_term run =
  let combine force = run @@ { force } in
  Term.(const combine $ force_term)

let cmd_doc = "Initialise hisoka"
let cmd_man = [ `S Manpage.s_description; `P "Initialise hisoka" ]

let cmd run =
  let info = Cmd.info name ~doc:cmd_doc ~man:cmd_man in
  Cmd.v info (cmd_term run)

let run cmd_init =
  let open Config in
  let open Util.FileSys in
  let force = cmd_init.force in
  let ( >>= ) = Result.bind in
  let app_path = Util.Path.to_string hisoka_dir in
  let is_app_folder_exist = Sys.file_exists app_path in
  let () =
    if is_app_folder_exist && not force then
      raise Error.(hisoka_error HisokaFolderAlreadyExist)
  in

  let () =
    if is_app_folder_exist && force then
      rmrf app_path ()
  in
  let first_message = "Choose the master password : " in
  let confirm_message = "Confirm the master password : " in
  let encrypted_key =
    match
      Input.confirm_password_encrypted ~first_message ~confirm_message ()
    with
    | Ok encrypted_key ->
        encrypted_key
    | Error exn ->
        raise (Input.PassError exn)
  in
  let res =
    create_folder ~on_error:(Error.CreateFolderError hisoka_dir) hisoka_dir
    >>= fun app_dir ->
    let external_file_path = Util.Path.push hisoka_rc app_dir in
    let manager = Manager.empty in
    let data = Manager.encrypt_with_changes ~key:encrypted_key [] manager in
    create_file
      ~on_file:(fun oc -> output_string oc data)
      ~on_error:(Error.CreateFileError external_file_path) external_file_path
    >>= fun external_file_path ->
    let app_dir = Util.Path.pop external_file_path in
    let data_foler_dir = Util.Path.push data_folder app_dir in
    create_folder ~on_error:(Error.CreateFolderError data_foler_dir)
      data_foler_dir
  in
  match res with
  | Ok _ ->
      Printf.printf "Hisoka initialized\n"
  | Error e ->
      raise (Error.hisoka_error e)

let command = cmd run
