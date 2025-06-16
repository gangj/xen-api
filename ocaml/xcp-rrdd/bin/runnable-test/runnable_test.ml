(*
 * Copyright (C) Cloud Software Group
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

module D = Debug.Make (struct let name = "runnable_test" end)

let ( -- ) = Int64.sub
let ( ++ ) = Int64.add
let ( // ) = Int64.div

let print_vcpus_runnable_time domid vcpus_runnable_times =
  Array.iteri
    (fun i time ->
      D.debug "domain %d, vcpu %d, total runnable time: %Ld" domid i time
    )
    vcpus_runnable_times

let _ =
  Xenctrl.with_intf (fun xc ->
      let domid =
        if Array.length Sys.argv = 2 then
          match Sys.argv.(1) |> int_of_string_opt with Some i -> i | None -> 1
        else
          1
      in
      D.debug "Check vCPU runnable time in domain %d" domid ;

      let nr_vcpus = (Xenctrl.domain_getinfo xc domid).Xenctrl.max_vcpu_id + 1 in
      D.debug "vCPU number in domain %d: %d" domid nr_vcpus ;

      (* get vcpuinfo of all vCPUs of the domain, which includes running time of the vCPUs *)
      let vcpus_runnable_times_domctl =
        Array.init nr_vcpus (fun i ->
            Xenctrl.domain_get_vcpurunnabletime xc domid i
        )
      in

      D.debug "=== runnable times of vCPUs from domctl hypercall for each vCPU" ;
      print_vcpus_runnable_time domid vcpus_runnable_times_domctl ;

      let avg_vcpus_runnable_time_domctl =
        (Array.fold_left (fun acc runnable ->
            acc ++ runnable
        ) 0L vcpus_runnable_times_domctl) // (Int64.of_int nr_vcpus)
      in
      D.debug "domain %d, average runnable time calculated from XEN_DOMCTL_getvcpurunnabletime: %Ld" domid avg_vcpus_runnable_time_domctl;

      (* get avg runnable time of all vCPUs of the domain *)
      let avg_runnable_time =
        (Xenctrl.domain_get_runstate_info xc domid).Xenctrl.avg_runnable
      in

      D.debug "=== Avg runnable time of vCPUs from XEN_DOMCTL_get_runstate_info:" ;
      D.debug "domain %d, average runnable time: %Ld" domid avg_runnable_time ;

      let diff = avg_vcpus_runnable_time_domctl -- avg_runnable_time in
      D.debug "=== Diff is %Ld nanoseconds, which is %f seconds" diff (Int64.to_float diff /. 1.0e9) ;

      D.debug "End"
  )
