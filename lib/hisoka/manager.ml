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

module External_Manager = struct
  let encryption_iv = String.init 12 (fun index -> Char.chr ( (index + 1) mod 256))

  type external_manager = {
    external_items: Items.External_Item.extern_item list
  }[@@deriving yojson]


  let create = { external_items = [] }

  
  (**
    @returns: The manager extended with the items
    @raise: [Hisoka_Error Already_Existing_name] if the id is already in the manager
  *)
  let append item manager = 
    match List.find_opt (fun bitem -> bitem |> Items.External_Item.compare item |> ( = ) 0) manager.external_items with 
    | None -> { external_items = item::manager.external_items }
    | Some _ -> raise (Error.HisokaError (Error.Already_Existing_name item.info.name))

  let append_list items manager = 
    items |> List.fold_left (fun acc elt -> 
      append elt acc
  ) manager
  

  let to_string manager = 
    manager |> external_manager_to_yojson |> Yojson.Safe.to_string

  let of_string bytes =
    bytes |> Yojson.Safe.from_string |> external_manager_of_yojson 

  let encrypt ~key external_manager () = 
    let data = to_string external_manager in
    let where = App.AppLocation.hisoka_extern_config_file |> PathBuf.to_string |> Option.some in
    Encryption.encrypt ~where ~key ~iv:encryption_iv data ()

    let filter ?(group = None) ~files manager = 
      let open Items.Item_Info in 
      let open Items.External_Item in
    {
      external_items = manager.external_items |> List.filter (fun eitem -> 
        match group with
        | Some _ -> group = eitem.info.group && files |> List.exists ( ( = ) eitem.info.name )
        | None -> files |> List.exists ( ( = ) eitem.info.name )
      )
    }

  let decrypt ~key () =
    let path = PathBuf.to_string  App.AppLocation.hisoka_extern_config_file in
    match Encryption.decrpty_file ~key ~iv:encryption_iv path () with
    | Error exn -> raise exn
    | Ok None -> raise (Error.HisokaError (Error.DecryptionError "Cannot decrypt external") )
    | Ok Some external_maneger_bytes -> begin 
      match of_string external_maneger_bytes with
      | Ok data -> data
      | Error e -> raise (Error.HisokaError (Error.DecryptionError e))
    end 

end

module Monolitchic_Manager = struct
  let encryption_iv = String.init 12 (fun index -> Char.chr ( (index + 3) mod 256))

  type monolithic_manager = {
    base_items: Items.Base_Item.base_item list
  }[@@deriving yojson]


  (**
    Append an item to the manager
    @return: The manager extended with the items
    @raise: [Hisoka_Error Already_Existing_name] if the id is already in the manager
  *)
  let append item manager = 
    match List.find_opt (fun bitem -> bitem |> Items.Base_Item.compare item |> ( <> ) 0) manager.base_items with 
    | None -> { base_items = item::manager.base_items }
    | Some _ -> raise (Error.HisokaError (Error.Already_Existing_name item.info.name))


  let replace item manager =
  let replaced, items = manager.base_items |> List.fold_left_map (fun acc bitem ->
    let find = Items.Base_Item.compare item bitem = 0 in
    match find with
    | true -> true, item
    | false -> acc, bitem
  ) false in
  if replaced 
    then { base_items = items }
  else 
    { base_items = item::items }

  let append_list items manager = {
    base_items = items |> List.fold_left (fun acc elt -> elt::acc) manager.base_items
  }

  let to_string manager = 
    manager |> monolithic_manager_to_yojson |> Yojson.Safe.to_string

  let of_string bytes =
    bytes |> Yojson.Safe.from_string |> monolithic_manager_of_yojson 

  let create = { base_items = [] }

  let filter  ?(group = None) ~files manager = 
  let open Items.Item_Info in 
  let open Items.Base_Item in
  {
    base_items = manager.base_items |> List.filter (fun bitem -> 
      match group with
      | Some _ -> group = bitem.info.group && files |> List.exists ( ( = ) bitem.info.name )
      | None -> files |> List.exists ( ( = ) bitem.info.name )
    )
  }

  let encrypt ~key external_manager () = 
    let data = to_string external_manager in
    let where = App.AppLocation.hisoka_mono_file |> PathBuf.to_string |> Option.some in
    Encryption.encrypt ~where ~key ~iv:encryption_iv data ()


  let decrypt ~key () =
    let path = PathBuf.to_string  App.AppLocation.hisoka_mono_file in
    match Encryption.decrpty_file ~key ~iv:encryption_iv path () with
    | Error exn -> raise exn
    | Ok None -> raise (Error.HisokaError (Error.DecryptionError "Cannot decrypt external") )
    | Ok Some monolithic_manager -> begin 
      match of_string monolithic_manager with
      | Ok data -> data
      | Error e -> raise (Error.HisokaError (Error.DecryptionError e))
    end 
  
end

module Manager = struct 

  type commit = {
    group: string option;
    name: string;
    extension: string;
    plain_data: string;
  }
  type manager = {
    external_manager: External_Manager.external_manager;
    monolithic_manager: Monolitchic_Manager.monolithic_manager;
    register_external_change: commit list;
  }

  exception Existing_Files of commit list

  let list_info manager = 
    (manager.monolithic_manager.base_items |> List.map (fun bi -> bi.Items.Base_Item.info) )
    @
    (manager.external_manager.external_items |> List.map (fun ei -> ei.Items.External_Item.info) )

  let add_item_from_file ~monolithic ?(group = None) ~name ~extension ~file_name manager = 
    let file = In_channel.(open_gen [Open_rdonly; Open_binary] 0 file_name) in
    let content = Util.read_file file () in
    In_channel.close file;
    if content = "" 
      then manager 
    else
      if monolithic 
        then 
          let item = Items.Base_Item.create ~group ~name ~extension content in
          { manager with monolithic_manager = Monolitchic_Manager.append item manager.monolithic_manager }
        else 
          let commit = { group; name; extension; plain_data = content} in
          { manager with register_external_change = commit::manager.register_external_change}

  (**
  @return: filter the manager by [group] and [files] 
  *)
  let fetch_group_files ?(group = None) ~files manager = 
    {
      manager with
      external_manager = External_Manager.filter ~group ~files manager.external_manager;
      monolithic_manager = Monolitchic_Manager.filter ~group ~files manager.monolithic_manager
    }

  (**
    @return: Decrypt all the files encrypted in the manager:
    @raise: [HisokaError Missing_file] if a file which was external encrypted is missing
  *)
  let decrypt_files ~dir_path ~key manager () =   
    let () = manager.monolithic_manager.base_items |> List.iter (fun base_item -> 
      let open Items.Base_Item in
      let filepath = Filename.concat dir_path base_item.info.name in
      Out_channel.with_open_bin filepath (fun oc -> 
        Printf.fprintf oc "%s" base_item.data
      )
    ) in

    let () = manager.external_manager.external_items |> List.iter (fun external_item -> 
      let open Items.External_Item in
      let outfile_path = Filename.concat dir_path external_item.info.name in
      let input_files_data = PathBuf.to_string @@ PathBuf.push external_item.encrypted_file_name App.AppLocation.hisoka_data_dir in
      let data = Encryption.decrpty_file ~key ~iv:external_item.iv input_files_data () in
      match data with
      | Error exn -> raise exn
      | Ok (None) -> raise (Error.(HisokaError (DecryptionError input_files_data) ) )
      | Ok (Some decrypted_data) ->
        Out_channel.with_open_bin outfile_path (fun oc -> 
          Printf.fprintf oc "%s" decrypted_data
        )
    ) in
    ()

  let is_empty manager = 
    manager.monolithic_manager.base_items = [] && manager.external_manager.external_items = []

  let encrypt ~max_iter ~raise_on_conflicts ~key manager () = 
    let pathbuf = App.AppLocation.hisoka_data_dir in
    let good_files, commit_conficts = manager.register_external_change |> List.partition_map (fun commit -> 
      let file_name = Util.generate_unique_name ~max_iter ~extension:commit.extension ~name:commit.name pathbuf in
      match file_name with
      | Some s -> Either.left (s, commit) (* md5 filename, groupe, plaindata*)
      | None -> Either.right commit
    ) in
    let () = if 
      raise_on_conflicts && commit_conficts <> [] then
        raise (Existing_Files commit_conficts) 
    in 

    let external_items, to_encrypted_data = good_files |> List.map (fun (encrypted_name, commit) -> 
      let item = Items.External_Item.create ~group:commit.group ~name:commit.name ~extension:commit.extension encrypted_name in
      item, (encrypted_name, item.iv, commit.plain_data)
    ) |> List.split in
    
    let {external_manager; monolithic_manager; register_external_change = _} = manager in
    let extented_external_manager = External_Manager.append_list external_items external_manager in
    let _ = External_Manager.encrypt ~key extented_external_manager () in
    let _ = Monolitchic_Manager.encrypt ~key monolithic_manager () in
    let _ = to_encrypted_data |> List.iter (fun (encrypted_name, iv, plain_data) -> 
      let where = App.AppLocation.hisoka_data_dir |> PathBuf.push encrypted_name |> PathBuf.to_string |> Option.some in
      let _ = Encryption.encrypt ~where ~key ~iv plain_data () in
      ()
    ) in 
    ()

    let decrypt ~key () = 
      let monolithic = Monolitchic_Manager.decrypt ~key () in
      let external_manager = External_Manager.decrypt ~key () in
      {
        monolithic_manager = monolithic;
        external_manager;
        register_external_change = []
      }
end