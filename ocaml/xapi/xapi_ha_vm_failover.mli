(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
(**
 * @group High Availability (HA)
*)

val all_protected_vms : __context:Context.t -> (API.ref_VM * API.vM_t) list

val restart_auto_run_vms :
  __context:Context.t -> API.ref_host list -> int -> unit
(** Take a set of live VMs and attempt to restart all protected VMs which have failed *)

val compute_evacuation_plan :
     __context:Context.t
  -> int
  -> API.ref_host list
  -> (API.ref_VM * API.vM_t) list
  -> (API.ref_VM * API.ref_host) list
(** Compute a plan for Host.evacuate *)

(** Abstract result of the background HA planning function *)
type plan_status =
  | Plan_exists_for_all_VMs  (** All protected VMs could be restarted *)
  | Plan_exists_excluding_non_agile_VMs
      (** Excluding 'trivial' failures due to non-agile VMs, all protected VMs could be restarted *)
  | No_plan_exists  (** Not all protected VMs could be restarted *)

(** Passed to the planner to reason about other possible configurations, used to block operations which would
    destroy the HA VM restart plan. *)
type configuration_change = {
    old_vms_leaving: (API.ref_host * (API.ref_VM * API.vM_t)) list
        (** existing VMs which are leaving *)
  ; old_vms_arriving: (API.ref_host * (API.ref_VM * API.vM_t)) list
        (** existing VMs which are arriving *)
  ; hosts_to_disable: API.ref_host list  (** hosts to pretend to disable *)
  ; num_failures: int option  (** new number of failures to consider *)
  ; new_vms_to_protect: API.ref_VM list  (** new VMs to restart *)
}

val no_configuration_change : configuration_change

val update_pool_status :
  __context:Context.t -> ?live_set:API.ref_host list -> unit -> bool
(** Update the Pool.ha_* fields with the current planning status *)

val plan_for_n_failures :
     __context:Context.t
  -> all_protected_vms:(API.ref_VM * API.vM_t) list
  -> ?live_set:API.ref_host list
  -> ?change:configuration_change
  -> int
  -> plan_status
(** Consider all possible failures of 'n' hosts *)

val compute_max_host_failures_to_tolerate :
     __context:Context.t
  -> ?live_set:API.ref_host list
  -> ?protected_vms:(API.ref_VM * API.vM_t) list
  -> unit
  -> int64
(** Compute the maximum plan size we can currently find *)

val assert_vm_placement_preserves_ha_plan :
     __context:Context.t
  -> ?leaving:(API.ref_host * (API.ref_VM * API.vM_t)) list
  -> ?arriving:(API.ref_host * (API.ref_VM * API.vM_t)) list
  -> unit
  -> unit
(** HA admission control functions: aim is to block operations which would make us become overcommitted: *)

val assert_host_disable_preserves_ha_plan :
  __context:Context.t -> API.ref_host -> unit

val assert_nfailures_change_preserves_ha_plan :
  __context:Context.t -> int -> unit

val assert_new_vm_preserves_ha_plan : __context:Context.t -> API.ref_VM -> unit

(* Below exposed only for ease of testing *)

module VMGroupMap : Map.S with type key = [`VM_group] Ref.t

module HostsSet : Set.S with type elt = int * int64 * API.ref_host

type anti_affinity_spread_evenly_pool_state = {
    host_size_map: int64 Xapi_vm_helpers.HostMap.t
  ; grp_host_vm_cnt_map: int Xapi_vm_helpers.HostMap.t VMGroupMap.t
  ; grp_hosts_set: HostsSet.t VMGroupMap.t
}

type anti_affinity_no_breach_pool_state = {
    host_size_map': int64 Xapi_vm_helpers.HostMap.t
  ; grp_no_resident_hosts_set: HostsSet.t VMGroupMap.t
  ; grp_resident_hosts_count_map: int VMGroupMap.t
}

val vm_anti_affinity_spread_evenly_plan :
     __context:Context.t
  -> anti_affinity_spread_evenly_pool_state
  -> (API.ref_VM -> API.ref_host -> bool)
  -> (API.ref_VM * int64 * API.ref_VM_group) list
  -> (API.ref_VM * API.ref_host) list
  -> (API.ref_VM * API.ref_host) list

val vm_anti_affinity_no_breach_plan :
     __context:Context.t
  -> int
  -> anti_affinity_no_breach_pool_state
  -> (API.ref_VM -> API.ref_host -> bool)
  -> (API.ref_VM * int64 * API.ref_VM_group) list
  -> (API.ref_VM * API.ref_host) list
  -> (API.ref_VM * API.ref_host) list

val vm_anti_affinity_evacuation_plan :
     __context:Context.t
  -> int
  -> (API.ref_host * int64) list
  -> (API.ref_VM * int64) list
  -> (API.ref_VM -> API.ref_host -> bool)
  -> (API.ref_VM * API.ref_host) list

val host_free_memory : __context:Context.t -> host:[`host] Ref.t -> int64

val vm_memory : __context:Context.t -> API.vM_t -> int64

val anti_affinity_vms_increasing :
     __context:Context.t
  -> (API.ref_VM * 'a) list
  -> (API.ref_VM * 'a * API.ref_VM_group) list

val init_anti_affinity_spread_evenly_pool_state :
     __context:Context.t
  -> (API.ref_host * int64) list
  -> anti_affinity_spread_evenly_pool_state

val init_anti_affinity_no_breach_pool_state :
  anti_affinity_spread_evenly_pool_state -> anti_affinity_no_breach_pool_state
