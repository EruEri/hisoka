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

type t = { iv : string; info : Info.t; encrypted_file_name : string }
[@@deriving yojson]

let compare lhs rhs = compare lhs.info rhs.info

let create ?iv ~groups ~name encrypted_file_name =
  let iv = match iv with Some iv -> iv | None -> Encryption.random_iv () in
  { iv; info = Info.create ~groups name; encrypted_file_name }

let to_string item = item |> to_yojson |> Yojson.Safe.to_string

let of_string bytes =
  bytes |> Yojson.Safe.from_string |> of_yojson |> Result.get_ok

let info item = item.info
