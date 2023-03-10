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

module AppLocation = struct
  let app_folder = ".hisoka"

  let data_folder = ".data"
  let config_file = ".hisoka_extern_rc"
  let monolithic_file = ".hisokamono"
  let hisoka_dir = PathBuf.from_list [Util.home_dir; app_folder]
  let hisoka_extern_config_file = PathBuf.push config_file hisoka_dir
  let hisoka_mono_file = PathBuf.push monolithic_file hisoka_dir
  let hisoka_data_dir = PathBuf.push data_folder hisoka_dir
end

module App = struct

  let is_app_folder_exist = 
    let open AppLocation in
    let app_path = hisoka_dir |> PathBuf.to_string in
    app_path |> Sys.file_exists

  let check_app_initialized () = 
    let () = if not is_app_folder_exist then
      raise Error.(HisokaError Hisoka_Not_Initialized)
    in
    ()

  let create_folder ?(perm = 0o700) ~on_error folder = 
    let to_path_string = PathBuf.to_string folder in
    match Sys.mkdir to_path_string perm with
    | exception _ -> 
      Error on_error
    | () -> Ok folder

  let create_file ?(on_file = fun _ -> ()) ~on_error  file = 
    let to_file_path = PathBuf.to_string file in
    match Out_channel.open_bin to_file_path with
    | exception _ -> Error on_error
    | outchan -> 
      let () = on_file outchan in
      let () =  close_out outchan in 
      Ok file

    let rec rmrf path () = 
      match Sys.is_directory path with
    | true ->
      Sys.readdir path |>
      Array.iter (fun name -> rmrf (Filename.concat path name) ());
      Unix.rmdir path
    | false -> Sys.remove path
end
