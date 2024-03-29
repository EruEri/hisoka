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

type error = No_matching_Password | Empty_Password

exception PassError of error

let confirm_password ~first_message ~confirm_message () =
  let pass1 = Cbindings.Input.getpass first_message () in
  let pass2 = Cbindings.Input.getpass confirm_message () in
  if pass1 <> pass2 then
    Error No_matching_Password
  else
    Ok pass1

let confirm_password_encrypted ~first_message ~confirm_message () =
  confirm_password ~first_message ~confirm_message ()
  |> Result.map (fun pass -> Encryption.aes_string_encrypt pass ())

let ask_password_encrypted ~prompt () =
  let pass = Cbindings.Input.getpass prompt () in
  Encryption.aes_string_encrypt pass ()

type _ input_behavior =
  | Continue : string option -> bool input_behavior
  | Stop_Wrong : string option -> bool option input_behavior

let rec confirm_choice :
    type a.
    continue_on_wrong_input:a input_behavior ->
    ?case_insensible:bool ->
    yes:char ->
    no:char ->
    prompt:string ->
    unit ->
    a =
 fun ~continue_on_wrong_input ?(case_insensible = true) ~yes ~no ~prompt () ->
  let string_transform =
    if case_insensible then
      String.lowercase_ascii
    else
      Fun.id
  in
  let () = Printf.printf "%s [%c/%c]: " prompt yes no in
  let choice = read_line () in
  let choice = string_transform choice in
  let s_yes = yes |> Char.escaped |> string_transform in
  let s_no = no |> Char.escaped |> string_transform in
  match continue_on_wrong_input with
  | Continue message ->
      if choice = s_yes then
        true
      else if choice = s_no then
        false
      else
        let () = Option.iter (Printf.printf "%s\n") message in
        confirm_choice ~continue_on_wrong_input ~case_insensible ~yes ~no
          ~prompt ()
  | Stop_Wrong message ->
      if choice = s_yes then
        Some true
      else if choice = s_no then
        Some false
      else
        let () = Option.iter (fun s -> Printf.printf "%s\n" s) message in
        None
