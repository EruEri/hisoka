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

open Cmdliner
open Util

module Common = struct
  let file_term ~docv ~doc = 
    let linfo = 
      Arg.info []
      ~docv
      ~doc
    in
    Arg.(non_empty & pos_all non_dir_file [] & linfo )

  let groups_term ~docv ~doc = 
    let ginfo =
      Arg.info ["g"; "group"]
      ~docv
      ~doc
    in
    Arg.(
      value & opt_all string [] & ginfo 
    )

    let stragtegy_group_term = 
      Arg.(
        required
        & opt (some & enum Strategy.strategy_group_enum) None
        & info ~docv:(Format.string_of_enum Strategy.strategy_group_enum)
            ~doc:"filter strategy" [ "s"; "strategy" ]
      )

end

module Init_Cmd = struct

  let name = "init"

  type cmd_init = {
    force: bool
  }

  type t = cmd_init

  let force_term = 
    let info = 
      Arg.info ["f"; "force"]
      ~doc:"force the initialisation"
    in
  Arg.value (Arg.flag info)

  let cmd_term run = 
    let combine force = 
      run @@ {force}
    in
    Term.(const combine 
      $ force_term
  )

  let cmd_doc = "Initialise hisoka app"
  let cmd_man = [
    `S Manpage.s_description;
    `P "Initialise hisoka app";
  ]

  let cmd run =
    let info =
      Cmd.info name
        ~doc:cmd_doc
        ~man:cmd_man
    in
    Cmd.v info (cmd_term run)


  let run cmd_init = 
    let open App.App in
    let open App.AppLocation in
    let force = cmd_init.force in
    let (>>=) = Result.bind in
    let app_path = PathBuf.to_string hisoka_dir in
    let is_app_folder_exist = Sys.file_exists app_path in
    let res = if is_app_folder_exist && not force
      then Error Error.App_folder_already_exist
    else
      let () = if is_app_folder_exist && force then 
        rmrf app_path ()
      in
      let first_message = "Choose the master password : " in
      let confirm_message = "Confirm the master password : " in
      let encrypted_key = match Input.confirm_password_encrypted ~first_message ~confirm_message () with
        | Ok encrypted_key -> encrypted_key
        | Error exn -> raise (Input.PassError exn) 
      in
      create_folder ~on_error:(Error.Create_folder hisoka_dir) hisoka_dir
      >>= fun app_dir -> 
        let external_file_path = PathBuf.push hisoka_rc app_dir in
        let external_manager = Manager.External_Manager.create in
        let data = Manager.External_Manager.encrypt ~key:encrypted_key external_manager () in
        create_file ~on_file:(fun oc -> output_string oc data) ~on_error:(Error.Create_file external_file_path) external_file_path
      >>= fun external_file_path -> 
        let app_dir = PathBuf.pop external_file_path in
        let data_foler_dir = PathBuf.push data_folder app_dir in
        create_folder ~on_error:(Error.Create_folder data_foler_dir) data_foler_dir 
    in
    match res with
    | Ok _ -> 
      Printf.printf "Hisoka initialized\n"
    | Error init_error -> raise (Error.HisokaError (Init_Error init_error) )
      

    let command = cmd run
end

module Add_Cmd = struct
  let name = "add"

  type cmd_add = {
    groups: string list;
    existing_groups: string list;
    files: string list;
  }

  type t = cmd_add

  let existing_group_term = 
      let einfo =
        Arg.info ["g"; "group"]
        ~docv:"EXISTING_GROUP"
        ~doc:"Append the file to a group of file. The group must exist"
      in
      Arg.(
        value & opt_all string [] & einfo 
      )


    let cmd_term run = 
      let combine files existing_groups groups = 
        run @@ { files; existing_groups; groups}
      in
      Term.(const combine
        $ Common.file_term ~docv:"Files" ~doc:("Files to add to hisoka")
        $ existing_group_term
        $ Common.groups_term ~docv:"groups" ~doc:""
    )

    let cmd_doc = "Add file to hisoka"
    let cmd_man = [
      `S Manpage.s_description;
      `P "Add file to hisoka";
    ]

    let cmd run =
      let info =
        Cmd.info name
          ~doc:cmd_doc
          ~man:cmd_man
      in
      Cmd.v info (cmd_term run)

    let run cmd_add = 
      let module StringSet = Util.StringSet in
      let {groups; existing_groups; files} = cmd_add in
      let encrypted_key = Input.ask_password_encrypted ~prompt:"Enter the master password : " () in
      let manager = Manager.Manager.decrypt ~key:encrypted_key () in
      (* checkher if existing groups exists  *)

      let groups = groups |> List.rev_append existing_groups |> StringSet.of_list |> StringSet.elements in
      let manager = files |> List.fold_left (fun manager file ->
        let name, extension = Filename.basename file, Filename.extension file in 
        Manager.Manager.add_item_from_file ~groups:groups ~name ~extension ~file_name:file manager
      ) manager in 
      let () = Manager.Manager.encrypt ~key:encrypted_key ~max_iter:3 ~raise_on_conflicts:true manager () in

      let () = List.iter (Printf.printf "File: \"%s\" Added\n") files in
      ()


    let command = cmd run
      
end

module List_Cmd = struct

  let name = "list"

  type cmd_list = {
    strategy: Strategy.strategy_group;
    groups: string list; 
  }

  type t = cmd_list

  let cmd_term run = 
    let combine strategy groups = 
      run @@ {strategy; groups}
    in
    Term.(const combine
      $ Common.stragtegy_group_term
      $ Common.groups_term ~docv:"" ~doc:""
    )

    let cmd_doc = "Display the list of encrypted files"
    let cmd_man = [
      `S Manpage.s_description;
      `P "Display the list of encrypted files";
    ]

    let cmd run =
      let info =
        Cmd.info name
          ~doc:cmd_doc
          ~man:cmd_man
      in
      Cmd.v info (cmd_term run)


    let run cmd_list = 
      let module StringSet = Util.StringSet in
      let () = App.App.check_app_initialized () in
      let {strategy; groups} = cmd_list in 
      let encrypted_key = Input.ask_password_encrypted ~prompt:"Enter the master password : " () in
      let manager = Manager.Manager.decrypt ~key:encrypted_key () in
      let items_list = Manager.Manager.list_info manager in
      let items_list = match groups with
      | [] -> items_list
      | groups -> 
        let filter_groups = StringSet.of_list groups in
        items_list |> List.filter (fun item -> 
          let open Items.Item_Info in
          let igroups = StringSet.of_list item.groups in
          Strategy.fstrategy strategy igroups filter_groups
        )
      in
      let () = items_list |> List.iter (fun info -> 
        let open Items.Item_Info in
        Printf.printf "group : [%s], name : %s, extension : %s\n" (String.concat ", " groups) info.name info.extension 
      )  in
      ()

    let command = cmd run
end

module Decrypt_Cmd = struct
  let name = "decrypt"

  type cmd_decrypt = {
    groups: string list;
    strategy: Strategy.strategy_group;
    out_dir: string option;
    files: string list;
  }

  type t = cmd_decrypt

  let files_term = 
    Arg.(non_empty & pos_all string [] & info [] ~docv:"FILES" )

  let out_dir_term = 
    Arg.(value 
      & opt (some dir) None 
      & info 
        ~docv:"DIRECTORY" 
        ~absent:"current directory" 
        ~doc:"Write decrypted files in the given $(b,DIRECTORY)" 
        ["out-dir"] 
    )

  let cmd_term run = 
    let combine groups strategy files out_dir = 
      run @@ {groups; strategy; files; out_dir}
    in
    Term.(const combine
      $ Common.groups_term ~docv:""  ~doc:"Decrypt all files belonging to GROUP"
      $ Common.stragtegy_group_term
      $ Common.file_term ~docv:"FILES" ~doc:"Decrypt all the files"
      $ out_dir_term
    )

    let cmd_doc = "Decrypt encrypted files"
    let cmd_man = [
      `S Manpage.s_description;
      `P "Decrypt encrypted files";
    ]

    let cmd run =
      let info =
        Cmd.info name
          ~doc:cmd_doc
          ~man:cmd_man
      in
      Cmd.v info (cmd_term run)

    let run decrypt_cmd = 
      let () = App.App.check_app_initialized () in
      let {groups; strategy; files; out_dir} = decrypt_cmd in
      let encrypted_key = Input.ask_password_encrypted ~prompt:"Enter the master password : " () in
      let manager = Manager.Manager.decrypt ~key:encrypted_key () in
      let manager_filtered = Manager.Manager.fetch_group_files
        ~strategy
        ~groups
        ~files
        manager
    in
    match Manager.Manager.is_empty manager_filtered with
    | false ->
      let outdir = Option.value ~default:(Sys.getcwd ()) out_dir in
      let () = Manager.Manager.decrypt_files ~dir_path:outdir ~key:encrypted_key manager_filtered () in
      ()
    | true -> raise (Error.(HisokaError No_file_to_decrypt))

    let command = cmd run
end

module Delete_Cmd = struct
  let name = "delete"

  type cmd_delete = {
    groups: string list;
    strategy: Strategy.strategy_group;
    files: string list;
  }

  type t = cmd_delete

  let files_term = 
    Arg.(value & pos_all string [] & info [] ~docv:"FILES" ~doc:"Files to delete")

  let group_term = 
    Arg.(value & opt (some string) None & info ["g"; "group"] ~docv:"GROUP" )

  let cmd_term run = 
      let combine groups strategy files = 
        run @@ {groups; strategy; files}
      in
      Term.(const combine
        $ Common.groups_term ~docv:"GROUP" ~doc:"Delete files belonging to GROUP. If no$(b, FILES) is provided, $(opt) will delete all$(b, FILES) in $(docv)"
        $ Common.stragtegy_group_term
        $ files_term
      )

  let cmd_doc = "Delete files from hisoka"

    let cmd_man = [
      `S Manpage.s_description;
      `P "Delete files";
    ]

    let cmd run =
      let info =
        Cmd.info name
          ~doc:cmd_doc
          ~man:cmd_man
      in
      Cmd.v info (cmd_term run)

    let run delete_cmd =
      let () = App.App.check_app_initialized () in
      let {groups; strategy; files} = delete_cmd in
      let delete_all_in_group = groups <> [] && delete_cmd.files = [] in
      let string_groups = String.concat ", " groups in
      let s =  (match groups with [] | _::[] -> "" | _ -> "s") in
      let prompt = match strategy with
        | Strategy.All -> 
          Printf.sprintf "Are you sure about deleting all files contains [%s] as group%s"
          string_groups
          s
        | Any -> 
          Printf.sprintf "Are you sure about deleting all files contains at least one of the follwing group%s: [%s]"
          s
          string_groups
        | Exact -> 
          Printf.sprintf "Are you sure about deleting all files with exacttly the ollwing group%s: [%s]"
          s
          string_groups
      in
      let () = if delete_all_in_group then
        let continue = Input.confirm_choice ~continue_on_wrong_input:(Input.Continue (Some "Wrong input") ) ~case_insensible:true ~yes:'y' ~no:'n' ~prompt () in
        if not continue then 
          raise Error.(HisokaError No_Option_choosen)
      in
      let encrypted_key = Input.ask_password_encrypted ~prompt:"Enter the master password : " () in
      let manager = Manager.Manager.decrypt ~key:encrypted_key () in
      let filtered_manager, deleted_files_info = Manager.Manager.remove ~strategy ~groups files manager in
      match deleted_files_info with
      | [] -> print_endline "No files to delete"
      | deleted_files_info ->
        let string_of_files = let open Items.Item_Info in (deleted_files_info |> List.map (fun {name; _} -> name) |> String.concat ", ")  in
        let deleting_file_format = 
          Printf.sprintf "Following files will be deleted : %s " string_of_files
        in
        let confirmed_deletion = Input.confirm_choice ~continue_on_wrong_input:(Input.Continue (Some "Wrong input") ) ~case_insensible:true ~yes:'y' ~no:'n' ~prompt:deleting_file_format () in
        if confirmed_deletion then
          let () = Manager.Manager.encrypt ~max_iter:3 ~raise_on_conflicts:true ~key:encrypted_key filtered_manager () in
          Printf.printf "Deleted : %s\n" string_of_files
        else
          raise Error.(HisokaError No_Option_choosen)
      

    let command = cmd run
end

module Display_Cmd = struct
  let name = "display"

  type display_cmd = {
    pixel_mode: Cbindings.Display.pixel_mode;
    strategy: Strategy.strategy_group;
    groups: string list;
    files: string list;
  }

  let pixel_modes =
    [ 
      ("iterm", Cbindings.Display.Iterm); 
      ("kitty", Cbindings.Display.Kitty); 
      ("sixel", Cbindings.Display.SIXEL); 
      ("none", Cbindings.Display.NONE);  
    ]

  let files_term = 
    Arg.(value & pos_all string [] & info [] ~docv:"FILES" ~doc:"Files to display")

  let pixel_mode = Arg.(
      required
      & opt ~vopt:(Some Cbindings.Display.NONE) (some & enum pixel_modes) (Some Cbindings.Display.NONE)
      & info ~docv:"Pixel Mode"
          ~doc:("Specify the pixel mode to use to render the image. " ^ doc_alts_enum ~quoted:true pixel_modes)
          [ "m"; "mode" ])

  let cmd_term run = 
    let combine pixel_mode strategy groups files = 
      run @@ {pixel_mode; strategy; groups; files}
    in
    Term.(const combine
      $ pixel_mode
      $ Common.stragtegy_group_term
      $ Common.groups_term ~docv:"GROUP" ~doc:"Render files belonging to GROUP"
      $ files_term
    )


  let cmd_doc = "Display files in the terminals"

  let cmd_man = [
    `S Manpage.s_description;
    `P "Display allows you to show files within hisoka directly in the terminal";
  ]

  let cmd run =
    let info =
      Cmd.info name
        ~doc:cmd_doc
        ~man:cmd_man
    in
    Cmd.v info (cmd_term run)

  let run cmd = 
    let () = App.App.check_app_initialized () in
    let {pixel_mode; strategy; groups; files} = cmd in
    let encrypted_key = Input.ask_password_encrypted ~prompt:"Enter the master password : " () in
    let manager = Manager.Manager.decrypt ~key:encrypted_key () in
    let items_list = Manager.Manager.list_name_data ~strategy ~key:encrypted_key ~groups manager in
    let items_list = match files with
      | [] -> items_list
      | files -> items_list |> List.filter (fun (s, _) -> List.mem s files) 
    in 
    let () = Cbindings.Display.hisoka_show items_list (List.length items_list) pixel_mode () in
    ()

  let command = cmd run
end


module Chafa_Test = struct
  type test_cmd = {
    file: string
  }

  let file_term = Arg.(required & pos 0 (some file) None & info ~doc:"Path to the picture file" [] )

  let cmd_term run = 
    let combine file = 
      run @@ {file}
    in
    Term.(const combine
      $ file_term
    )

    let cmd_doc = "Test Chafa"

    let cmd_man = [
      `S Manpage.s_description;
      `P "Test Chafa output";
    ]

    let cmd run =
      let info =
        Cmd.info "chafa"
          ~doc:cmd_doc
          ~man:cmd_man
      in
      Cmd.v info (cmd_term run)

      let run {file} = 
        let () = Cbindings.Chafa_Test.chafa_display file () in
        ()

  let command = cmd run
end



module Hisoka_Cmd = struct
  let version = "alpha"
  let root_doc = "the file hidder"

  type t = bool

  let change_term = Arg.(value & flag & info ["change-master-password"] ~doc:"Change the master password")

  let cmd_term run = 
    let combine change = 
      run @@ change
    in
    Term.(const combine
      $ change_term
    )

  let run_base change_password = 
    ignore change_password;
    ()


  let root_man = [
    `S Manpage.s_description;
    `P "hisoka allows you to encrypt any file";
  ]

  let root_info =
    Cmd.info "hisoka"
      ~doc:root_doc
      ~man:root_man
      ~version
  let subcommands = [
    Init_Cmd.command;
    Add_Cmd.command;
    List_Cmd.command;
    Decrypt_Cmd.command;
    Delete_Cmd.command;
    Chafa_Test.command;
    Display_Cmd.command;
  ]

  let parse () =
    Cmd.group ~default:(cmd_term run_base) root_info subcommands

  let eval () = () |> parse |> Cmd.eval
end