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

module StringSet = Set.Make (String)

module Strategy = struct
  type strategy_group = Any | All | Exact

  let strategy_group_enum = [ ("any", Any); ("all", All); ("exact", Exact) ]

  let fstrategy = function
    | Any ->
        fun lhs rhs -> not @@ StringSet.disjoint lhs rhs
    | All ->
        StringSet.subset
    | Exact ->
        StringSet.equal
end

module Hash = struct
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
      let file_full_path = PathBuf.to_string @@ PathBuf.push hashed_name path in
      if not @@ Sys.file_exists file_full_path then
        Some hashed_name
      else
        generate_unique_name ~max_iter:(max_iter - 1) ~name:hashed_name
          ~extension path
end

module Format = struct
  let string_of_enum ?(splitter = "|") ?(quoted = false) enum =
    let f =
      if quoted then
        Cmdliner.Arg.doc_quote
      else
        Fun.id
    in
    enum |> List.map (fun (elt, _) -> f elt) |> String.concat splitter
end

module Io = struct
  let read_file ch () = really_input_string ch (in_channel_length ch)
end

module FileSys = struct
  let create_folder ?(perm = 0o700) ~on_error folder =
    let to_path_string = PathBuf.to_string folder in
    match Sys.mkdir to_path_string perm with
    | exception _ ->
        Error on_error
    | () ->
        Ok folder

  let create_file ?(on_file = fun _ -> ()) ~on_error file =
    let to_file_path = PathBuf.to_string file in
    match Out_channel.open_bin to_file_path with
    | exception _ ->
        Error on_error
    | outchan ->
        let () = on_file outchan in
        let () = close_out outchan in
        Ok file

  let rec rmrf path () =
    match Sys.is_directory path with
    | true ->
        Sys.readdir path
        |> Array.iter (fun name -> rmrf (Filename.concat path name) ());
        Unix.rmdir path
    | false ->
        Sys.remove path
    | exception e ->
        raise e
end
