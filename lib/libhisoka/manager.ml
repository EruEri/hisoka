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

module StringSet = Util.Strategy.StringSet

module External_Manager = struct
  let encryption_iv =
    String.init 12 (fun index -> Char.chr ((index + 1) mod 256))

  type external_manager = { external_items : Item.t list } [@@deriving yojson]

  let create = { external_items = [] }

  (**
    @return : The manager extended with the items
    @raise Hisoka_Error.AlreadyExistingName if the id is already in the manager
*)
  let append item manager =
    match
      List.find_opt
        (fun bitem -> bitem |> Item.compare item |> ( = ) 0)
        manager.external_items
    with
    | None ->
        { external_items = item :: manager.external_items }
    | Some _ ->
        raise (Error.HisokaError (Error.AlreadyExistingName item.info.name))

  let append_list items manager =
    items |> List.fold_left (fun acc elt -> append elt acc) manager

  let to_string manager =
    manager |> external_manager_to_yojson |> Yojson.Safe.to_string

  let of_string bytes =
    bytes |> Yojson.Safe.from_string |> external_manager_of_yojson

  let encrypt ~key external_manager () =
    let data = to_string external_manager in
    let where =
      Option.some @@ Util.Path.to_string Config.hisoka_extern_config_file
    in
    Encryption.encrypt ~where ~key ~iv:encryption_iv data ()

  (** 
      [exist f list] is the as same as [List.exists] except that if the list is empty
      it return true
    *)
  let exist f = function [] -> true | files -> List.exists f files

  (**
      [filter ~fstrategy ~groups ~files manager] filters [manager] according to the [groups] and files
      if [files] is empty, are the files in [manager] are matched
    *)
  let filter ~fstrategy ~groups ~files manager =
    let open Info in
    let open Item in
    {
      external_items =
        manager.external_items
        |> List.filter (fun eitem ->
               let is_exist = exist (( = ) eitem.info.name) files in
               match groups with
               | [] ->
                   is_exist
               | groups ->
                   let egroups = StringSet.of_list eitem.info.groups in
                   let filter_groups = StringSet.of_list groups in
                   fstrategy filter_groups egroups && is_exist
           );
    }

  let exclude ~fstrategy ~groups ~files manager =
    let open Info in
    let open Item in
    let external_items, deleted =
      manager.external_items
      |> List.partition_map (fun eitem ->
             let is_matched = exist (( = ) eitem.info.name) files in
             match groups with
             | [] ->
                 if not is_matched then
                   Either.left eitem
                 else
                   Either.right eitem
             | groups -> (
                 let egroups = StringSet.of_list eitem.info.groups in
                 let filter_groups = StringSet.of_list groups in
                 match fstrategy filter_groups egroups && is_matched with
                 | true ->
                     Either.right eitem
                 | false ->
                     Either.left eitem
               )
         )
    in
    ({ external_items }, deleted)

  (**
    excluded all the items in [manager] which are contained in [groups]
  *)
  let exclude_group ~fstrategy ~groups manager =
    let open Info in
    let open Item in
    let external_items, deleted =
      manager.external_items
      |> List.partition_map (fun eitem ->
             match groups with
             | [] ->
                 Either.right eitem
             | groups ->
                 let egroups = StringSet.of_list eitem.info.groups in
                 let filter_groups = StringSet.of_list groups in
                 if fstrategy filter_groups egroups then
                   Either.right eitem
                 else
                   Either.left eitem
         )
    in
    ({ external_items }, deleted)

  let decrypt ~key () =
    let path = Util.Path.to_string Config.hisoka_extern_config_file in
    match Encryption.decrpty_file ~key ~iv:encryption_iv path () with
    | Error exn ->
        raise exn
    | Ok None ->
        raise
          (Error.HisokaError (Error.DecryptionError "Cannot decrypt external"))
    | Ok (Some external_maneger_bytes) -> (
        match of_string external_maneger_bytes with
        | Ok data ->
            data
        | Error e ->
            raise (Error.HisokaError (Error.DecryptionError e))
      )
end

module Manager = struct
  type commit = {
    groups : string list;
    name : string;
    extension : string;
    plain_data : string;
  }

  type manager = {
    external_manager : External_Manager.external_manager;
    register_external_change : commit list;
  }

  exception Existing_Files of commit list

  let infos manager = List.map Item.info manager.external_manager.external_items

  (**
      @raise HisokaError.MissingFile if a file which was external encrypted is missing
  *)
  let list_name_data ~strategy ~key ~groups manager =
    manager.external_manager.external_items
    |> List.filter_map (fun ei ->
           let open Item in
           let input_files_data =
             Util.Path.to_string
             @@ Util.Path.push ei.encrypted_file_name Config.hisoka_data_dir
           in
           let data =
             Encryption.decrpty_file ~key ~iv:ei.iv input_files_data ()
           in
           match data with
           | Error exn ->
               raise exn
           | Ok None ->
               raise Error.(HisokaError (DecryptionError input_files_data))
           | Ok (Some decrypted_data) -> (
               match groups with
               | [] ->
                   Some (ei.info.name, decrypted_data)
               | groups -> (
                   let filter_groups = StringSet.of_list groups in
                   let items_groups = StringSet.of_list ei.info.groups in
                   match
                     Util.Strategy.fstrategy strategy filter_groups items_groups
                   with
                   | true ->
                       Some (ei.info.name, decrypted_data)
                   | false ->
                       None
                 )
             )
       )

  let add_item_from_file ~groups ~name ~extension ~file_name manager =
    let file = In_channel.(open_gen [ Open_rdonly; Open_binary ] 0 file_name) in
    let content = Util.Io.read_file file in
    let () = In_channel.close file in
    if content = String.empty then
      manager
    else
      let commit = { groups; name; extension; plain_data = content } in
      {
        manager with
        register_external_change = commit :: manager.register_external_change;
      }

  (**
  @return : filter the manager by [group] and [files] 
*)
  let fetch_group_files ~strategy ~groups ~files manager =
    {
      manager with
      external_manager =
        External_Manager.filter
          ~fstrategy:(Util.Strategy.fstrategy strategy)
          ~groups ~files manager.external_manager;
    }

  (**
    @return Decrypt all the files encrypted in the manager:
    @raise HisokaError.MissingFile if a file which was external encrypted is missing
  *)
  let decrypt_files ~dir_path ~key manager () =
    let () =
      manager.external_manager.external_items
      |> List.iter (fun external_item ->
             let open Item in
             let outfile_path =
               Filename.concat dir_path external_item.info.name
             in
             let input_files_data =
               Util.Path.to_string
               @@ Util.Path.push external_item.encrypted_file_name
                    Config.hisoka_data_dir
             in
             let data =
               Encryption.decrpty_file ~key ~iv:external_item.iv
                 input_files_data ()
             in
             match data with
             | Error exn ->
                 raise exn
             | Ok None ->
                 raise
                 @@ Error.(hisoka_error @@ DecryptionError input_files_data)
             | Ok (Some decrypted_data) ->
                 Out_channel.with_open_bin outfile_path (fun oc ->
                     Printf.fprintf oc "%s" decrypted_data
                 )
         )
    in
    ()

  let is_empty manager = manager.external_manager.external_items = []

  let encrypt ~max_iter ~raise_on_conflicts ~key manager () =
    let path = Config.hisoka_data_dir in
    let good_files, commit_conficts =
      manager.register_external_change
      |> List.partition_map (fun commit ->
             let file_name =
               Util.Hash.generate_unique_name ~max_iter
                 ~extension:commit.extension ~name:commit.name path
             in
             match file_name with
             | Some s ->
                 Either.left (s, commit) (* md5 filename, groupe, plaindata*)
             | None ->
                 Either.right commit
         )
    in
    let () =
      if raise_on_conflicts && commit_conficts <> [] then
        raise (Existing_Files commit_conficts)
    in

    let external_items, to_encrypted_data =
      good_files
      |> List.map (fun (encrypted_name, commit) ->
             let item =
               Item.create ~groups:commit.groups ~name:commit.name
                 encrypted_name
             in
             (item, (encrypted_name, item.iv, commit.plain_data))
         )
      |> List.split
    in

    let { external_manager; register_external_change = _ } = manager in
    let extented_external_manager =
      External_Manager.append_list external_items external_manager
    in
    let _ = External_Manager.encrypt ~key extented_external_manager () in
    let _ =
      to_encrypted_data
      |> List.iter (fun (encrypted_name, iv, plain_data) ->
             let where =
               Config.hisoka_data_dir
               |> Util.Path.push encrypted_name
               |> Util.Path.to_string |> Option.some
             in
             let _ = Encryption.encrypt ~where ~key ~iv plain_data () in
             ()
         )
    in
    ()

  let remove ~strategy ~groups files manager =
    match (groups, files) with
    | _, [] ->
        let external_manager, ex_deleted =
          External_Manager.exclude_group
            ~fstrategy:(Util.Strategy.fstrategy strategy)
            ~groups manager.external_manager
        in
        ({ manager with external_manager }, ex_deleted)
    | _ ->
        let external_manager, ex_deleted =
          External_Manager.exclude
            ~fstrategy:(Util.Strategy.fstrategy strategy)
            ~groups ~files manager.external_manager
        in
        ({ manager with external_manager }, ex_deleted)

  let decrypt ~key () =
    let external_manager = External_Manager.decrypt ~key () in
    { external_manager; register_external_change = [] }

  let groups manager =
    manager.external_manager.external_items
    |> List.fold_left
         (fun set item ->
           let open Item in
           item.info.groups |> StringSet.of_list |> StringSet.union set
         )
         StringSet.empty
    |> StringSet.elements
end
