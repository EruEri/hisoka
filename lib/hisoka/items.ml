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


module Item_Info = struct
  type info = {
    groups: string list;
    name: string;
    extension: string
  }[@@deriving yojson]

  let create ~groups ~extension name = 
    {
      groups;
      name;
      extension
    }
end

module Base_Item = struct
  type base_item = {
    info: Item_Info.info;
    data: string
  }[@@deriving yojson]

  let create ~groups ~name ~extension data = {
    info = Item_Info.create ~groups ~extension name;
    data
  }

  let compare lhs rhs = compare lhs.info rhs.info

  let to_string item =
  item |> base_item_to_yojson |> Yojson.Safe.to_string 

  let of_string bytes = 
    bytes |> Yojson.Safe.from_string |> base_item_of_yojson |> Result.get_ok
end


module External_Item = struct
  type extern_item = {
    iv: string;
    info: Item_Info.info;
    encrypted_file_name: string
  }[@@deriving yojson]

  let compare lhs rhs = compare lhs.info rhs.info

  let create ?iv ~groups ~name ~extension encrypted_file_name = 
    let iv = match iv with Some iv -> iv | None -> Encryption.random_iv in
    {
      iv;
      info = Item_Info.create ~groups ~extension name;
      encrypted_file_name
    }


  let to_string item =
    item |> extern_item_to_yojson |> Yojson.Safe.to_string 
  
  let of_string bytes = 
    bytes |> Yojson.Safe.from_string |> extern_item_of_yojson |> Result.get_ok

end