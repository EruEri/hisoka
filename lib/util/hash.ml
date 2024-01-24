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

let hash_name ~name ~extension =
  let extension =
    if extension = String.empty then
      String.empty
    else
      "." ^ extension
  in
  let hash_name = name ^ extension |> Digest.string |> Digest.to_hex in
  Printf.sprintf "%s" hash_name

let rec generate_unique_name ?(max_iter = 3) ~extension ~name path =
  if max_iter <= 0 then
    None
  else
    let hashed_name = hash_name ~name ~extension in
    let file_full_path = Path.to_string @@ Path.push hashed_name path in
    if not @@ Sys.file_exists file_full_path then
      Some hashed_name
    else
      generate_unique_name ~max_iter:(max_iter - 1) ~name:hashed_name
        ~extension path