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

type error = 
| DecryptionError of string
| Already_Existing_name of string

let string_of_error = function
| DecryptionError file -> Printf.sprintf "decrptytion error : %s" file
| Already_Existing_name filename -> Printf.sprintf "Filename : \"%s\" is already in hisoka" filename

exception HisokaError of error

let register_exn () = Printexc.register_printer (function
| HisokaError error -> error |> string_of_error |> Option.some
| _ -> None 
)

let register_exn = register_exn ()