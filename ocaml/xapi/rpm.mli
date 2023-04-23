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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *)

(** The representation of Epoch concept in RPM versioning *)
module Epoch : sig
  type t = int option

  val epoch_none : string

  val of_string : string -> t

  val to_string : t -> string
end

(** The representation of a RPM Name-Version-Release (NVR) *)
module Pkg : sig
  type t = {
      name: string
    ; epoch: Epoch.t
    ; version: string
    ; release: string
    ; arch: string
  }

  type order

  val string_of_order : order -> string

  val to_name_arch_string : t -> string

  val to_fullname : t -> string

  val of_fullname : string -> t option

  val compare_version_strings : string -> string -> order

  val to_epoch_ver_rel_json : t -> Yojson.Basic.t

  (* <op> epoch1 version1 release1 epoch2 version2 release2
   * Compare two RPM versions in form of <epoch>:<version>-<release>.
   * lt: return true only if <epoch1>:<version1>-<release1> is older than <epoch2>:<version2>-<release2>
   * gt: return true only if <epoch1>:<version1>-<release1> is newer than <epoch2>:<version2>-<release2>
   * eq: return true only if <epoch1>:<version1>-<release1> is same as <epoch2>:<version2>-<release2>
   * lte: lt || eq
   * gte: gt || eq
   *)

  val lt : Epoch.t -> string -> string -> Epoch.t -> string -> string -> bool

  val gt : Epoch.t -> string -> string -> Epoch.t -> string -> string -> bool

  val eq : Epoch.t -> string -> string -> Epoch.t -> string -> string -> bool

  val lte : Epoch.t -> string -> string -> Epoch.t -> string -> string -> bool

  val gte : Epoch.t -> string -> string -> Epoch.t -> string -> string -> bool

  val parse_epoch_version_release : string -> Epoch.t * string * string
end

val get_latest_version_release :
  (string * string) list -> (string * string) option
(** [get_latest_version_release vrs] is the latest version-release in [vrs]
   * or [None] if [vrs] is empty. *)
