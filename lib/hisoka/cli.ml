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
      run @@ `Init {force}
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
    let app_path = hisoka_dir |> PathBuf.to_string in
    let is_app_folder_exist = app_path |> Sys.file_exists in
    let res = if is_app_folder_exist && not force
      then Error App_folder_already_exist
    else
      let () = if is_app_folder_exist && force then 
        rmrf app_path ()
      in
      let first_message = "Choose the master password : " in
      let confirm_message = "Confirm the master password : " in
      let encrypted_key = match Pass.confirm_password_encrypted ~first_message ~confirm_message () with
        | Ok encrypted_key -> encrypted_key
        | Error exn -> raise (Pass.PassError exn) 
      in
      create_folder ~on_error:(Create_folder hisoka_dir) hisoka_dir
      >>= fun hisoka_dir -> 
        let monolithic_file_path = PathBuf.push monolithic_file hisoka_dir in
        let monolithic_manager = Manager.Monolitchic_Manager.create in
        let data =  Manager.Monolitchic_Manager.encrypt ~key:encrypted_key monolithic_manager () in
        create_file ~on_file:(fun oc -> output_string oc data) ~on_error:(Create_file monolithic_file_path) monolithic_file_path
      >>= fun monolithic_file_path ->
        let app_dir = PathBuf.pop monolithic_file_path in
        let external_file_path = PathBuf.push config_file app_dir in
        let external_manager = Manager.External_Manager.create in
        let data = Manager.External_Manager.encrypt ~key:encrypted_key external_manager () in
        create_file ~on_file:(fun oc -> output_string oc data) ~on_error:(Create_file external_file_path) external_file_path
      >>= fun external_file_path -> 
        let app_dir = PathBuf.pop external_file_path in
        let data_foler_dir = PathBuf.push data_folder app_dir in
        create_folder ~on_error:(Create_folder data_foler_dir) data_foler_dir 
    in
    match res with
    | Ok _ -> 
      Printf.printf "Hisoka initialized\n"
    | Error init_error -> raise (Init_Error init_error)
      

end

module Add_Cmd = struct
  let name = "add"

  type cmd_add = {
    monolithic: bool;
    group: string option;
    download_url: string option;
    file: string;
  }

  type t = cmd_add

  let monolithic_term =
    let info = 
      Arg.info ["m"; "mono"; "monolithic"]
      ~doc:"Whenever the encrypted should be merge within one big file containing others encrypted file"
    in
    Arg.value (Arg.flag info)

  let file_term = 
    let info = 
      Arg.info []
      ~docv:"FILE"
      ~doc:"file to encrypt"
    in
    Arg.required  ( Arg.pos  0 (Arg.some Arg.file) None info) 

  let download_url_term = 
    let info = 
      Arg.info ["url"]
      ~docv:"URL"
      ~doc:"Url of the file"
    in
    Arg.value (Arg.opt (Arg.some Arg.string) None info )

  let group_term = 
    let info =
      Arg.info ["g"; "group"]
      ~docv:"GROUP"
      ~doc:"Append the file to a group of file"
    in
    Arg.value (Arg.opt (Arg.some Arg.string) None info)

    let cmd_term run = 
      let combine monolithic file download_url group = 
        run @@ `Add {monolithic; file; download_url; group}
      in
      Term.(const combine 
        $ monolithic_term
        $ file_term
        $ download_url_term
        $ group_term
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
      let encrypted_key = Pass.ask_password_encrypted ~prompt:"Enter the master password : " () in
      let manager = Manager.Manager.decrypt ~key:encrypted_key () in
      let () = print_endline "decrypted" in
      let manager = if 
          Option.is_some cmd_add.download_url 
        then 
          failwith "Downlaod urn not done" 
          else 
            let name, extension = Filename.basename cmd_add.file, Filename.extension cmd_add.file in 
            Manager.Manager.add_item_from_file ~monolithic:cmd_add.monolithic ~group:cmd_add.group ~name ~extension ~file_name:cmd_add.file manager
        in
      let () = print_endline "appended" in
      let () = Manager.Manager.encrypt ~key:encrypted_key ~max_iter:3 ~raise_on_conflicts:true manager () in
      Printf.printf "monolithic = %b: file = %s" cmd_add.monolithic cmd_add.file;
      
end

module List_Cmd = struct
  let name = "list"

  type cmd_list = {
    group: string option; 
  }

  type t = cmd_list

  let group_term = 
    let info =
      Arg.info ["g"; "group"]
      ~docv:"GROUP"
      ~doc:"List all the file belonging to $(SOURCE)"
    in
    Arg.value (Arg.opt (Arg.some Arg.string) None info)

  let cmd_term run = 
    let combine group = 
      run @@ `List {group}
    in
    Term.(const combine
      $ group_term
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
      let encrypted_key = Pass.ask_password_encrypted ~prompt:"Enter the master password : " () in
      let manager = Manager.Manager.decrypt ~key:encrypted_key () in
      let items_list = Manager.Manager.list_info manager in
      let items_list = match cmd_list.group with
      | None -> items_list
      | Some _ as group -> 
        items_list |> List.filter (fun item -> 
          let open Items.Item_Info in
          item.group = group
        )
      in
      let () = items_list |> List.iter (fun info -> 
        let open Items.Item_Info in
        Printf.printf "group : %s, name : %s, extension : %s\n" (Option.value ~default:"null" info.group) info.name info.extension 
      )  in
      ()
end



module Hisoka_Cmd = struct
  let version = "alpha"
  let root_doc = "the file hidder"

  let root_man = [
    `S Manpage.s_description;
    `P "hisoka allows you to encrypt any file";
  ]

  let root_info =
    Cmd.info "hisoka"
      ~doc:root_doc
      ~man:root_man
      ~version
  let subcommands run = [
    Init_Cmd.cmd run;
    Add_Cmd.cmd run;
    List_Cmd.cmd run;
  ]


  let run = 
  function
  | `Init init_cmd -> Init_Cmd.run init_cmd
  | `Add add_cmd -> Add_Cmd.run add_cmd
  | `List list_cmd -> List_Cmd.run list_cmd

  let parse run =
    Cmd.group root_info (subcommands run)

  let eval () = run |> parse |> Cmd.eval
end