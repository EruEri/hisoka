(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of Hisoka                                                                *)
(* Copyright (C) 2023 Yves Ndiaye                                                             *)
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

open Util

type error =
  | NoOptionChoosen
  | NoFileToDecrypt
  | HisokaNotInitialized
  | DecryptionError of string
  | AlreadyExistingName of string
  | MissingFile of { true_name : string; encrypted_name : string }
  | HisokaFolderAlreadyExist
  | CreateFolderError of Path.t
  | CreateFileError of Path.t
  | EncryptionError of Path.t
  | NoneExistingGroup of string list
  | ExistingFiles of Commit.t list

let string_of_error = function
  | HisokaNotInitialized ->
      Printf.sprintf
        "\".hisoka\" directory doesn't exist. Use hisoka init to initialize"
  | NoOptionChoosen ->
      "Operation Aborted"
  | NoFileToDecrypt ->
      Printf.sprintf "No File to decrypt"
  | DecryptionError file ->
      Printf.sprintf "decrptytion error : %s" file
  | AlreadyExistingName filename ->
      Printf.sprintf "Filename : \"%s\" is already in hisoka" filename
  | MissingFile { true_name; encrypted_name } ->
      Printf.sprintf "Filename: \"%s\" is missing: This file encrypted: \"%s\""
        encrypted_name true_name
  | HisokaFolderAlreadyExist ->
      "\".hisoka\" directory already exists"
  | CreateFolderError path ->
      Printf.sprintf "Unable to create directory : %s" (Path.to_string path)
  | CreateFileError path ->
      Printf.sprintf "Unable to create file : %s" (Path.to_string path)
  | EncryptionError path ->
      Printf.sprintf "Unable to encrypt file : %s" (Path.to_string path)
  | NoneExistingGroup groups ->
      let s, does =
        match groups with [] | _ :: [] -> ("", "doesn't") | _ -> ("s", "don't")
      in
      Printf.sprintf "The following group%s %s exist: [%s]" s does
        (String.concat ", " groups)
  | ExistingFiles commits ->
      let s, does =
        match commits with
        | [] | _ :: [] ->
            ("", "doesn't")
        | _ ->
            ("s", "don't")
      in
      Printf.sprintf "The following name%s hash %s exist: [%s]" s does
        (String.concat ", "
        @@ List.map (fun commit -> commit.Commit.name) commits
        )

exception HisokaError of error

let hisoka_error e = HisokaError e

let register_exn () =
  Printexc.register_printer (function
    | HisokaError error ->
        error |> string_of_error |> Option.some
    | _ ->
        None
    )

let register_exn = register_exn ()
