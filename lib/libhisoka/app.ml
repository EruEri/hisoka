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
  let hisoka = "hisoka"
  let ( // ) = Filename.concat
  let xdg = Xdg.create ~env:Sys.getenv_opt ()
  let xdg_data = Xdg.data_dir xdg
  let hisoka_dir = xdg_data // hisoka
  let data_folder = ".data"
  let hisoka_rc = ".hisokarc"
  let hisoka_dir = Util.Path.from_list [ hisoka_dir ]
  let hisoka_extern_config_file = Util.Path.push hisoka_rc hisoka_dir
  let hisoka_data_dir = Util.Path.push data_folder hisoka_dir
end

module App = struct
  let is_app_folder_exist =
    let open AppLocation in
    let app_path = Util.Path.to_string hisoka_dir in
    Sys.file_exists app_path

  let check_app_initialized () =
    let () =
      if not is_app_folder_exist then
        raise Error.(HisokaError HisokaNotInitialized)
    in
    ()
end
