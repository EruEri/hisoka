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

type pathbuf = string list

let pop : pathbuf -> pathbuf = List.tl

let push: string -> pathbuf -> pathbuf = List.cons

let to_string pathbuf = pathbuf |> List.rev |> String.concat Filename.dir_sep

let create name: pathbuf = [name]
let from_list l = l |> List.rev 

let exists_in ~file ~pathbuf = Sys.file_exists (pathbuf |> push file |> to_string)
