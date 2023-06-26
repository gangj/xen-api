(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

module XenAPI = Client.Client
module Date = Xapi_stdext_date.Date

type message_name_t = string

type message_priority_t = int64

type message_id_t = message_name_t * message_priority_t

type remaining_days_t = int

type expiry_messaging_info_t = {
    cls:
      [ `Certificate
      | `Host
      | `PVS_proxy
      | `Pool
      | `SR
      | `VDI
      | `VM
      | `VMPP
      | `VMSS ]
  ; obj_uuid: string
  ; obj_description: string
  ; alert_conditions: (remaining_days_t * message_id_t) list
  ; expiry: Xapi_stdext_date.Date.t (* when the obj will expire *)
}

let seconds_per_day = 3600. *. 24.

let days_until_expiry epoch expiry =
  (expiry /. seconds_per_day) -. (epoch /. seconds_per_day)

let all_messages rpc session_id =
  XenAPI.Message.get_all_records ~rpc ~session_id

let message_body msg expiry =
  Printf.sprintf "<body><message>%s</message><date>%s</date></body>" msg
    (Date.to_string expiry)

let expired_message obj = Printf.sprintf "The %s has expired." obj

let expiring_message obj = Printf.sprintf "The %s is expiring soon." obj

let generate_alert now obj_description alert_conditions expiry =
  let remaining_days =
    days_until_expiry (Date.to_float now) (Date.to_float expiry)
  in
  alert_conditions
  |> List.sort (fun (a, _) (b, _) -> compare a b)
  |> List.find_opt (fun (days, _) -> remaining_days < float_of_int days)
  |> function
  | None ->
      None
  | Some (days, expired_message_id) when days <= 0 ->
      let expired = expired_message obj_description in
      Some (message_body expired expiry, expired_message_id)
  | Some (_, expiring_message_id) ->
      let expiring = expiring_message obj_description in
      Some (message_body expiring expiry, expiring_message_id)

let filter_messages msg_name_list msg_obj_uuid alert all_msgs =
  let msg_body, (msg_name, msg_prio) = alert in
  all_msgs
  |> List.filter (fun (_ref, record) ->
         record.API.message_obj_uuid = msg_obj_uuid
         && List.mem record.API.message_name msg_name_list
     )
  |> List.partition (fun (_ref, record) ->
         record.API.message_body <> msg_body
         || record.API.message_name <> msg_name
         || record.API.message_priority <> msg_prio
     )

let alert ~rpc ~session_id expiry_messaging_info_list =
  let now = Date.now () in
  let all_msgs = all_messages rpc session_id in
  List.iter
    (fun {cls; obj_uuid; obj_description; alert_conditions; expiry} ->
      let alert = generate_alert now obj_description alert_conditions expiry in
      match alert with
      | Some alert ->
          let msg_name_list =
            List.map (fun (_, (msg_name, _)) -> msg_name) alert_conditions
          in
          all_msgs |> filter_messages msg_name_list obj_uuid alert
          |> fun (outdated, current) ->
          List.iter
            (fun (self, _) -> XenAPI.Message.destroy ~rpc ~session_id ~self)
            outdated ;
          if current = [] then
            let body, (name, priority) = alert in
            XenAPI.Message.create ~rpc ~session_id ~name ~priority ~cls
              ~obj_uuid ~body
            |> ignore
      | None ->
          ()
    )
    expiry_messaging_info_list
