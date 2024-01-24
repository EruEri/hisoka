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

let file_term ~docv ~doc =
  let linfo = Arg.info [] ~docv ~doc in
  Arg.(non_empty & pos_all non_dir_file [] & linfo)

let groups_term ~docv ~doc =
  let ginfo = Arg.info [ "g"; "group" ] ~docv ~doc in
  Arg.(value & opt_all string [] & ginfo)

let stragtegy_group_term =
  let default = Some Util.Strategy.All in
  Arg.(
    required
    & opt ~vopt:default (some & enum Strategy.strategy_group_enum) default
    & info
        ~docv:(Format.string_of_enum Strategy.strategy_group_enum)
        ~doc:
          "the filter strategy to apply. \"any\" matches if at least one of \
           the given groups belongs to the group of the file. \"all\" \
           matches if all the given groups belongs to the group of the \
           file.  \"exact\" matches if exactly all the given groups are the \
           same groups as the file"
        [ "s"; "strategy" ]
  )