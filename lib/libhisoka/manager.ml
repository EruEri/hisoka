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

module ItemSet = Set.Make (Item)
module StringSet = Set.Make (String)

type internal = { external_items : Item.t list } [@@deriving yojson]
type t = { items : ItemSet.t }

let empty = { items = ItemSet.empty }
let encryption_iv = String.init 12 (fun index -> Char.chr ((index + 1) mod 256))
let to_internal manager = { external_items = ItemSet.elements manager.items }
let of_internal manager = { items = ItemSet.of_list manager.external_items }
let is_empty manager = ItemSet.is_empty manager.items

let add item manager =
  match ItemSet.find_opt item manager.items with
  | None ->
      { items = ItemSet.add item manager.items }
  | Some item ->
      raise Error.(hisoka_error @@ AlreadyExistingName item.info.name)

(**
    [change groups name extension filename]
*)
let change groups name extension filename =
  Commit.{ groups; name; extension; path = filename }

let add_seq items manager = Seq.fold_left (Fun.flip add) manager items
let to_string manager = Yojson.Safe.to_string @@ internal_to_yojson @@ manager
let of_string s = internal_of_yojson @@ Yojson.Safe.from_string s
let items manager = ItemSet.elements manager.items
let infos manager = List.map Item.info @@ items manager

let groups manager =
  let module StringSet = Set.Make (String) in
  StringSet.elements
  @@ ItemSet.fold
       (fun item acc -> StringSet.add_seq (List.to_seq item.info.groups) acc)
       manager.items StringSet.empty

let encrypt_manager ~key manager =
  let data = to_string manager in
  let where =
    Option.some @@ Util.Path.to_string Config.hisoka_extern_config_file
  in
  Encryption.encrypt ~where ~key ~iv:encryption_iv data

let decrypt ~key () =
  let path = Util.Path.to_string Config.hisoka_extern_config_file in
  match Encryption.decrpty_file ~key ~iv:encryption_iv path () with
  | Error exn ->
      raise exn
  | Ok None ->
      raise Error.(hisoka_error @@ DecryptionError "Cannot decrypt hisokarc")
  | Ok (Some external_maneger_bytes) -> (
      match of_string external_maneger_bytes with
      | Ok data ->
          of_internal data
      | Error e ->
          raise Error.(hisoka_error @@ DecryptionError e)
    )

(**
    [filter fstrategy groups files manager] filters [manager] according to the [groups] and files
    if [files] is empty, are the files in [manager] are matched
  *)
let filter fstrategy groups files manager =
  let open Item in
  let exists item = List.exists (( = ) item.info.name) in
  let filter exist groups item =
    match groups with
    | [] ->
        exist
    | groups ->
        let egroups = StringSet.of_list item.info.groups in
        let filter_groups = StringSet.of_list groups in
        fstrategy filter_groups egroups && exist
  in
  match (groups, files) with
  | [], [] ->
      manager
  | (_ :: _ as groups), [] ->
      let items = ItemSet.filter (filter true groups) manager.items in
      { items }
  | groups, files ->
      let items =
        ItemSet.filter
          (fun item ->
            let exist = exists item files in
            filter exist groups item
          )
          manager.items
      in
      { items }

let exclude fstrategy groups files manager =
  let open Info in
  let open Item in
  let exists item = files = [] || List.exists (( = ) item.info.name) files in
  let items, deleted =
    ItemSet.partition
      (fun item ->
        let is_matched = exists item in
        match groups with
        | [] ->
            not is_matched
        | groups ->
            let egroups = StringSet.of_list item.info.groups in
            let filter_groups = StringSet.of_list groups in
            not (fstrategy filter_groups egroups && is_matched)
      )
      manager.items
  in
  let deleted = ItemSet.elements deleted in
  ({ items }, deleted)

let exclude_group fstrategy groups manager =
  let open Info in
  let open Item in
  let items, deleted =
    ItemSet.partition
      (fun item ->
        match groups with
        | [] ->
            false
        | groups ->
            let egroups = StringSet.of_list item.info.groups in
            let filter_groups = StringSet.of_list groups in
            not @@ fstrategy filter_groups egroups
      )
      manager.items
  in
  let deleted = ItemSet.elements deleted in
  let manager = { items } in
  (manager, deleted)

let remove fstrategy groups files manager =
  match (groups, files) with
  | _, [] ->
      exclude_group (Util.Strategy.fstrategy fstrategy) groups manager
  | _ ->
      exclude (Util.Strategy.fstrategy fstrategy) groups files manager

let encrypt_with_changes ?(max_iter = 3) ?(raise_on_conflicts = true) ~key
    commits manager =
  let open Commit in
  let path = Config.hisoka_data_dir in
  let good_files, commit_conficts =
    List.partition_map
      (fun commit ->
        let file_name =
          Util.Hash.generate_unique_name ~max_iter ~extension:commit.extension
            ~name:commit.name path
        in
        match file_name with
        | Some s ->
            Either.left (s, commit) (* md5 filename, groupe, plaindata*)
        | None ->
            Either.right commit
      )
      commits
  in
  let () =
    if raise_on_conflicts && commit_conficts <> [] then
      raise Error.(hisoka_error @@ ExistingFiles commit_conficts)
  in
  let items, to_encrypt_data =
    List.split
    @@ List.map
         (fun (encrypted_name, commit) ->
           let item =
             Item.create ~groups:commit.groups ~name:commit.name encrypted_name
           in
           let plain_data =
             In_channel.with_open_bin commit.path Util.Io.read_file
           in
           (item, (encrypted_name, item.iv, plain_data))
         )
         good_files
  in
  let () =
    List.iter
      (fun (encrypted_name, iv, plain_data) ->
        let where =
          Config.hisoka_data_dir
          |> Util.Path.push encrypted_name
          |> Util.Path.to_string |> Option.some
        in
        ignore @@ Encryption.encrypt ~where ~key ~iv plain_data
      )
      to_encrypt_data
  in
  let manager = add_seq (List.to_seq items) manager in
  encrypt_manager ~key (to_internal manager)

(**
    [decrypt_all ~key dir_path manager] decrypts all the files encrypted in the manager
    @raise HisokaError.MissingFile if a file which was external encrypted is missing
*)
let decrypt_all ~key dir_path manager =
  let ( / ) = Filename.concat in
  ItemSet.iter
    (fun item ->
      let outfile = dir_path / item.info.name in
      let encrypted_file =
        Util.Path.to_string
        @@ Util.Path.push item.encrypted_file_name Config.hisoka_data_dir
      in
      let decrypted_data =
        Encryption.decrpty_file ~key ~iv:item.iv encrypted_file ()
      in
      match decrypted_data with
      | Error exn ->
          Printf.eprintf "Warning : %s\n" @@ Printexc.to_string exn
      | Ok None ->
          let message =
            Error.string_of_error @@ DecryptionError encrypted_file
          in
          Printf.eprintf "Warning : %s\n" message
      | Ok (Some decrypted_data) ->
          Out_channel.with_open_bin outfile (fun oc ->
              Printf.fprintf oc "%s" decrypted_data
          )
    )
    manager.items
