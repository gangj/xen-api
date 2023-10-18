let product_version () = Inventory.lookup ~default:"" "PRODUCT_VERSION"

let product_version_text () =
  Inventory.lookup ~default:"" "PRODUCT_VERSION_TEXT"

let product_version_text_short () =
  Inventory.lookup ~default:"" "PRODUCT_VERSION_TEXT_SHORT"

let platform_name () = Inventory.lookup ~default:"" "PLATFORM_NAME"

let platform_version () = Inventory.lookup ~default:"0.0.0" "PLATFORM_VERSION"

let product_brand () = Inventory.lookup ~default:"" "PRODUCT_BRAND"

let build_number () = Inventory.lookup ~default:"" "BUILD_NUMBER"

let hostname = "localhost"

let date = Xapi_build_info.date

let version, xapi_version_major, xapi_version_minor, git_id =
  match Build_info.V1.version () with
  | None ->
      ("0.0.dev", 0, 0, "dev")
  | Some v -> (
      let str = Build_info.V1.Version.to_string v in
      let version =
        if String.starts_with ~prefix:"v" str then
          String.sub str 1 (String.length str - 1)
        else
          str
      in
      try
        let maj, min, git_id =
          Scanf.sscanf version "%d.%d.%s" (fun maj min rest -> (maj, min, rest))
        in
        (version, maj, min, git_id)
      with _ ->
        failwith
          (Printf.sprintf
             "Couldn't determine xapi version - got unexpected version from \
              dune: '%s'"
             version
          )
    )

let compare_version version_a version_b =
  let maj_a, min_a, _ =
    try Scanf.sscanf version_a "%d.%d.%s" (fun maj min rest -> (maj, min, rest))
    with _ ->
      failwith
        (Printf.sprintf "Couldn't determine xapi version for version_a: '%s'"
           version_a
        )
  in
  let maj_b, min_b, _ =
    try Scanf.sscanf version_b "%d.%d.%s" (fun maj min rest -> (maj, min, rest))
    with _ ->
      failwith
        (Printf.sprintf "Couldn't determine xapi version for version_b: '%s'"
           version_b
        )
  in
  if maj_a > maj_b || (maj_a = maj_b && min_a > min_b) then
    1
  else if maj_a = maj_b && min_a = min_b then
    0
  else
    -1
