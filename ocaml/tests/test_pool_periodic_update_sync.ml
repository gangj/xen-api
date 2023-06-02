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

open Test_highlevel
open Pool_periodic_update_sync

module TestUpdateSyncDelay = struct
  let test_next_scheduled_datetime_now () =
    let utc_now = Ptime_clock.now () in
    let y, m, d, hh, mm, ss =
      next_scheduled_datetime ~delay:0. ~utc_now
        ~tz_offset_s:(Ptime_clock.current_tz_offset_s () |> Option.get)
    in
    let tm = Unix.localtime (Ptime.to_float_s utc_now) in
    Alcotest.(check int)
      "test_next_scheduled_datetime_now year" y (tm.tm_year + 1900) ;
    Alcotest.(check int)
      "test_next_scheduled_datetime_now month" m (tm.tm_mon + 1) ;
    Alcotest.(check int) "test_next_scheduled_datetime_now day" d tm.tm_mday ;
    Alcotest.(check int) "test_next_scheduled_datetime_now hour" hh tm.tm_hour ;
    Alcotest.(check int) "test_next_scheduled_datetime_now mintue" mm tm.tm_min ;
    Alcotest.(check int) "test_next_scheduled_datetime_now second" ss tm.tm_sec

  let test_next_scheduled_datetime_10_minutes_later () =
    let utc_now = Ptime_clock.now () in
    let y, m, d, hh, mm, ss =
      next_scheduled_datetime ~delay:(10. *. 60.) ~utc_now
        ~tz_offset_s:(Ptime_clock.current_tz_offset_s () |> Option.get)
    in
    let tm = Unix.localtime (Ptime.to_float_s utc_now +. (10. *. 60.)) in
    Alcotest.(check int)
      "test_next_scheduled_datetime_10_minutes_later year" y (tm.tm_year + 1900) ;
    Alcotest.(check int)
      "test_next_scheduled_datetime_10_minutes_later month" m (tm.tm_mon + 1) ;
    Alcotest.(check int)
      "test_next_scheduled_datetime_10_minutes_later day" d tm.tm_mday ;
    Alcotest.(check int)
      "test_next_scheduled_datetime_10_minutes_later hour" hh tm.tm_hour ;
    Alcotest.(check int)
      "test_next_scheduled_datetime_10_minutes_later mintue" mm tm.tm_min ;
    Alcotest.(check int)
      "test_next_scheduled_datetime_10_minutes_later second" ss tm.tm_sec

  let test_next_scheduled_datetime_tomorrow () =
    let utc_now = Ptime_clock.now () in
    let y, m, d, hh, mm, ss =
      next_scheduled_datetime
        ~delay:(24. *. 60. *. 60.)
        ~utc_now
        ~tz_offset_s:(Ptime_clock.current_tz_offset_s () |> Option.get)
    in
    let tm = Unix.localtime (Ptime.to_float_s utc_now +. (24. *. 60. *. 60.)) in
    Alcotest.(check int)
      "test_next_scheduled_datetime_tomorrow year" y (tm.tm_year + 1900) ;
    Alcotest.(check int)
      "test_next_scheduled_datetime_tomorrow month" m (tm.tm_mon + 1) ;
    Alcotest.(check int)
      "test_next_scheduled_datetime_tomorrow day" d tm.tm_mday ;
    Alcotest.(check int)
      "test_next_scheduled_datetime_tomorrow hour" hh tm.tm_hour ;
    Alcotest.(check int)
      "test_next_scheduled_datetime_tomorrow mintue" mm tm.tm_min ;
    Alcotest.(check int)
      "test_next_scheduled_datetime_tomorrow second" ss tm.tm_sec

  let test_utc_start_of_next_scheduled_day_1 () =
    let tz_offset_s = Ptime_clock.current_tz_offset_s () |> Option.get in
    let utc_now =
      Ptime.of_date_time ((2023, 5, 4), ((13, 14, 15), tz_offset_s))
      |> Option.get
    in
    let res =
      utc_start_of_next_scheduled_day ~utc_now ~tz_offset_s ~frequency:`daily
        ~day_configed_int:0
    in
    let utc_start_of_next_day =
      Ptime.of_date_time ((2023, 5, 5), ((0, 0, 0), tz_offset_s)) |> Option.get
    in
    Alcotest.(check bool)
      "test_utc_start_of_next_scheduled_day_1, daily" true
      (Ptime.equal res utc_start_of_next_day)

  let test_utc_start_of_next_scheduled_day_2 () =
    let tz_offset_s = Ptime_clock.current_tz_offset_s () |> Option.get in
    let utc_now =
      Ptime.of_date_time ((2023, 5, 31), ((13, 14, 15), tz_offset_s))
      |> Option.get
    in
    let res =
      utc_start_of_next_scheduled_day ~utc_now ~tz_offset_s ~frequency:`daily
        ~day_configed_int:0
    in
    let utc_start_of_next_day =
      Ptime.of_date_time ((2023, 6, 1), ((0, 0, 0), tz_offset_s)) |> Option.get
    in
    Alcotest.(check bool)
      "test_utc_start_of_next_scheduled_day_2, daily, end of month" true
      (Ptime.equal res utc_start_of_next_day)

  let test_utc_start_of_next_scheduled_day_3 () =
    let tz_offset_s = Ptime_clock.current_tz_offset_s () |> Option.get in
    let utc_now =
      Ptime.of_date_time ((2023, 12, 31), ((13, 14, 15), tz_offset_s))
      |> Option.get
    in
    let res =
      utc_start_of_next_scheduled_day ~utc_now ~tz_offset_s ~frequency:`daily
        ~day_configed_int:0
    in
    let utc_start_of_next_day =
      Ptime.of_date_time ((2024, 1, 1), ((0, 0, 0), tz_offset_s)) |> Option.get
    in
    Alcotest.(check bool)
      "test_utc_start_of_next_scheduled_day_3, daily, end of year" true
      (Ptime.equal res utc_start_of_next_day)

  let test_utc_start_of_next_scheduled_day_4 () =
    let tz_offset_s = Ptime_clock.current_tz_offset_s () |> Option.get in
    let utc_now =
      Ptime.of_date_time ((2023, 5, 4), ((13, 14, 15), tz_offset_s))
      |> Option.get
    in
    let res =
      utc_start_of_next_scheduled_day ~utc_now ~tz_offset_s ~frequency:`weekly
        ~day_configed_int:6
    in
    let utc_start_of_next_sched_day =
      Ptime.of_date_time ((2023, 5, 6), ((0, 0, 0), tz_offset_s)) |> Option.get
    in
    Alcotest.(check bool)
      "test_utc_start_of_next_scheduled_day_4, weekly, this week" true
      (Ptime.equal res utc_start_of_next_sched_day)

  let test_utc_start_of_next_scheduled_day_5 () =
    let tz_offset_s = Ptime_clock.current_tz_offset_s () |> Option.get in
    let utc_now =
      Ptime.of_date_time ((2023, 5, 4), ((13, 14, 15), tz_offset_s))
      |> Option.get
    in
    let res =
      utc_start_of_next_scheduled_day ~utc_now ~tz_offset_s ~frequency:`weekly
        ~day_configed_int:1
    in
    let utc_start_of_next_sched_day =
      Ptime.of_date_time ((2023, 5, 8), ((0, 0, 0), tz_offset_s)) |> Option.get
    in
    Alcotest.(check bool)
      "test_utc_start_of_next_scheduled_day_5, weekly, next week" true
      (Ptime.equal res utc_start_of_next_sched_day)

  let test_utc_start_of_next_scheduled_day_6 () =
    let tz_offset_s = Ptime_clock.current_tz_offset_s () |> Option.get in
    let utc_now =
      Ptime.of_date_time ((2023, 5, 4), ((13, 14, 15), tz_offset_s))
      |> Option.get
    in
    let res =
      utc_start_of_next_scheduled_day ~utc_now ~tz_offset_s ~frequency:`weekly
        ~day_configed_int:4
    in
    let utc_start_of_next_sched_day =
      Ptime.of_date_time ((2023, 5, 11), ((0, 0, 0), tz_offset_s)) |> Option.get
    in
    Alcotest.(check bool)
      "test_utc_start_of_next_scheduled_day_6, weekly, next week 2" true
      (Ptime.equal res utc_start_of_next_sched_day)

  let test_update_sync_delay_for_next_schedule_internal_1 () =
    let tz_offset_s = Ptime_clock.current_tz_offset_s () |> Option.get in
    let utc_now =
      Ptime.of_date_time ((2023, 5, 4), ((0, 0, 1), tz_offset_s)) |> Option.get
    in
    let utc_start_of_next_sched_day =
      Ptime.of_date_time ((2023, 5, 6), ((0, 0, 0), tz_offset_s)) |> Option.get
    in
    let extra_seconds = 0. in
    let delay = (2. *. 24. *. 60. *. 60.) -. 1. in
    let res = calc_delay ~utc_now ~utc_start_of_next_sched_day ~extra_seconds in
    Alcotest.(check (float Float.epsilon))
      "test_update_sync_delay_for_next_schedule_internal_1, extra_seconds 0"
      delay res

  let test_update_sync_delay_for_next_schedule_internal_2 () =
    let tz_offset_s = Ptime_clock.current_tz_offset_s () |> Option.get in
    let utc_now =
      Ptime.of_date_time ((2023, 5, 4), ((0, 0, 1), tz_offset_s)) |> Option.get
    in
    let utc_start_of_next_sched_day =
      Ptime.of_date_time ((2023, 5, 6), ((0, 0, 0), tz_offset_s)) |> Option.get
    in
    let extra_seconds = 66. in
    let delay = (2. *. 24. *. 60. *. 60.) -. 1. +. 66. in
    let res = calc_delay ~utc_now ~utc_start_of_next_sched_day ~extra_seconds in
    Alcotest.(check (float Float.epsilon))
      "test_update_sync_delay_for_next_schedule_internal_2, extra_seconds not 0"
      delay res

  let test_update_sync_delay_for_next_schedule_internal_3 () =
    let tz_offset_s = Ptime_clock.current_tz_offset_s () |> Option.get in
    let utc_now =
      Ptime.of_date_time ((2023, 4, 29), ((0, 0, 1), tz_offset_s)) |> Option.get
    in
    let utc_start_of_next_sched_day =
      Ptime.of_date_time ((2023, 5, 4), ((0, 0, 0), tz_offset_s)) |> Option.get
    in
    let extra_seconds = 66. in
    let delay = (5. *. 24. *. 60. *. 60.) -. 1. +. 66. in
    let res = calc_delay ~utc_now ~utc_start_of_next_sched_day ~extra_seconds in
    Alcotest.(check (float Float.epsilon))
      "test_update_sync_delay_for_next_schedule_internal_3, next month" delay
      res

  let test =
    [
      ("next_scheduled_datetime now", `Quick, test_next_scheduled_datetime_now)
    ; ( "next_scheduled_datetime 10 minutes later"
      , `Quick
      , test_next_scheduled_datetime_10_minutes_later
      )
    ; ( "next_scheduled_datetime tomorrow"
      , `Quick
      , test_next_scheduled_datetime_tomorrow
      )
    ; ( "utc_start_of_next_scheduled_day daily"
      , `Quick
      , test_utc_start_of_next_scheduled_day_1
      )
    ; ( "utc_start_of_next_scheduled_day daily, end of month"
      , `Quick
      , test_utc_start_of_next_scheduled_day_2
      )
    ; ( "utc_start_of_next_scheduled_day daily, end of year"
      , `Quick
      , test_utc_start_of_next_scheduled_day_3
      )
    ; ( "utc_start_of_next_scheduled_day weekly, this week"
      , `Quick
      , test_utc_start_of_next_scheduled_day_4
      )
    ; ( "utc_start_of_next_scheduled_day weekly, next week"
      , `Quick
      , test_utc_start_of_next_scheduled_day_5
      )
    ; ( "utc_start_of_next_scheduled_day weekly, next week, 2"
      , `Quick
      , test_utc_start_of_next_scheduled_day_6
      )
    ; ( "calc_delay, random seconds 0"
      , `Quick
      , test_update_sync_delay_for_next_schedule_internal_1
      )
    ; ( "calc_delay, random seconds not 0"
      , `Quick
      , test_update_sync_delay_for_next_schedule_internal_2
      )
    ; ( "calc_delay, next month"
      , `Quick
      , test_update_sync_delay_for_next_schedule_internal_3
      )
    ]
end

let tests =
  make_suite "pool_periodic_update_sync_"
    [("periodic_update_sync", TestUpdateSyncDelay.test)]

let () = Alcotest.run "Pool Periodic Update Sync" tests
