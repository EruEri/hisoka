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

type init_error =
  | App_folder_already_exist
  | Create_folder of Path.t
  | Create_file of Path.t
  | EncryptionError of Path.t

type error =
  | No_Option_choosen
  | No_file_to_decrypt
  | Hisoka_Not_Initialized
  | DecryptionError of string
  | Already_Existing_name of string
  | Missing_file of { true_name : string; encrypted_name : string }
  | Init_Error of init_error
  | Non_existing_group of string list

let string_of_init_error = function
  | App_folder_already_exist ->
      "\".hisoka\" directory already exists"
  | Create_folder path ->
      Printf.sprintf "Unable to create directory : %s" (Path.to_string path)
  | Create_file path ->
      Printf.sprintf "Unable to create file : %s" (Path.to_string path)
  | EncryptionError path ->
      Printf.sprintf "Unable to encrypt file : %s" (Path.to_string path)

let string_of_error = function
  | Hisoka_Not_Initialized ->
      Printf.sprintf
        "\".hisoka\" directory doesn't exist. Use hisoka init to initialize"
  | No_Option_choosen ->
      "Operation Aborted"
  | No_file_to_decrypt ->
      Printf.sprintf "No File to decrypt"
  | DecryptionError file ->
      Printf.sprintf "decrptytion error : %s" file
  | Already_Existing_name filename ->
      Printf.sprintf "Filename : \"%s\" is already in hisoka" filename
  | Missing_file { true_name; encrypted_name } ->
      Printf.sprintf "Filename: \"%s\" is missing: This file encrypted: \"%s\""
        encrypted_name true_name
  | Init_Error init ->
      string_of_init_error init
  | Non_existing_group groups ->
      let s, does =
        match groups with [] | _ :: [] -> ("", "doesn't") | _ -> ("s", "don't")
      in
      Printf.sprintf "The following group%s %s exist: [%s]" s does
        (String.concat ", " groups)

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
