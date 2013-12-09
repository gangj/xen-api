open Rrd_protocol

module Rrdp = Rrdp_common.Common(struct let name = "test_rrd_writer" end)

(* Field sizes. *)
let default_header = "DATASOURCES"

let header_bytes = String.length default_header

let data_checksum_bytes = 16

let metadata_checksum_bytes = 16

let datasource_count_bytes = 4

let timestamp_bytes = 8

let datasource_value_bytes = 8

let metadata_length_bytes = 4

let get_total_bytes datasource_count metadata_length =
	header_bytes +
	data_checksum_bytes +
	metadata_checksum_bytes +
	datasource_count_bytes +
	timestamp_bytes +
	(datasource_value_bytes * datasource_count) +
	(metadata_length * 8)

(* Field start points. *)
let header_start = 0

let data_checksum_start = header_start + header_bytes

let metadata_checksum_start = data_checksum_start + data_checksum_bytes

let datasource_count_start = metadata_checksum_start + metadata_checksum_bytes

let timestamp_start = datasource_count_start + datasource_count_bytes

let datasource_value_start = timestamp_start + timestamp_bytes

let get_metadata_length_start datasource_count =
	datasource_value_start + (datasource_count * datasource_value_bytes)

let get_metadata_start datasource_count =
	(get_metadata_length_start datasource_count) + metadata_length_bytes

(* Reading fields from cstructs. *)
module Read = struct
	let header cs =
		let header = String.create header_bytes in
		Cstruct.blit_to_string cs header_start header 0 header_bytes;
		header

	let data_checksum cs =
		let data_checksum = String.create data_checksum_bytes in
		Cstruct.blit_to_string cs data_checksum_start
			data_checksum 0 data_checksum_bytes;
		data_checksum

	let metadata_checksum cs =
		let metadata_checksum = String.create metadata_checksum_bytes in
		Cstruct.blit_to_string cs metadata_checksum_start
			metadata_checksum 0 metadata_checksum_bytes;
		metadata_checksum

	let datasource_count cs =
		Int32.to_int (Cstruct.BE.get_uint32 cs datasource_count_start)

	let timestamp cs =
		Cstruct.BE.get_uint64 cs timestamp_start

	let datasource_values cs cached_datasources =
		let rec aux start acc = function
			| [] -> acc
			| (cached_datasource, owner) :: rest -> begin
				(* Replace the cached datasource's value with the value read
				 * from the cstruct. *)
				let value = match cached_datasource.Ds.ds_value with
				| Rrd.VT_Float _ ->
					Rrd.VT_Float (Int64.float_of_bits (Cstruct.BE.get_uint64 cs start))
				| Rrd.VT_Int64 _ ->
					Rrd.VT_Int64 (Cstruct.BE.get_uint64 cs start)
				| Rrd.VT_Unknown -> failwith "unknown datasource type"
				in
				aux (start + datasource_value_bytes)
					(({cached_datasource with Ds.ds_value = value}, owner) :: acc)
					rest
			end
		in
		List.rev (aux datasource_value_start [] cached_datasources)

	let metadata_length cs datasource_count =
		Int32.to_int
			(Cstruct.BE.get_uint32 cs (get_metadata_length_start datasource_count))

	let metadata cs datasource_count metadata_length =
		let metadata = String.create metadata_length in
		Cstruct.blit_to_string
			cs (get_metadata_start datasource_count)
			metadata 0 metadata_length;
		metadata
end

(* Writing fields to cstructs. *)
module Write = struct
	let header cs =
		Cstruct.blit_from_string default_header 0 cs header_start header_bytes

	let data_checksum cs value =
		Cstruct.blit_from_string value 0
			cs data_checksum_start data_checksum_bytes

	let metadata_checksum cs value =
		Cstruct.blit_from_string value 0
			cs metadata_checksum_start metadata_checksum_bytes

	let datasource_count cs value =
		Cstruct.BE.set_uint32 cs datasource_count_start (Int32.of_int value)

	let timestamp cs value =
		Cstruct.BE.set_uint64 cs timestamp_start value

	let datasource_values cs values =
		let rec aux start = function
			| [] -> ()
			| value :: rest -> begin
				let to_write = match value with
				| Rrd.VT_Float f -> Int64.bits_of_float f
				| Rrd.VT_Int64 i -> i
				| Rrd.VT_Unknown -> failwith "unknown datasource type"
				in
				Cstruct.BE.set_uint64 cs start to_write;
				aux (start + datasource_value_bytes) rest
			end
		in
		aux datasource_value_start values

	let metadata_length cs value datasource_count =
		Cstruct.BE.set_uint32 cs
			(get_metadata_length_start datasource_count) (Int32.of_int value)

	let metadata cs value datasource_count =
		let metadata_length = String.length value in
		Cstruct.blit_from_string value 0 cs
			(get_metadata_start datasource_count)
			metadata_length
end

let last_data_checksum = ref ""
let last_metadata_checksum = ref ""
let cached_datasources : (Ds.ds * Rrd.ds_owner) list ref = ref []

let value_of_string (s : string) : Rrd.ds_value_type =
	match s with
	| "float" -> Rrd.VT_Float 0.0
	| "int64" -> Rrd.VT_Int64 0L
	| _ -> raise Invalid_payload

(* WARNING! This creates datasources from datasource metadata, hence the
 * values will be meaningless. The types however, will be correct. *)
let uninitialised_ds_of_rpc ((name, rpc) : (string * Rpc.t))
		: (Ds.ds * Rrd.ds_owner) =
	let open Rpc in
	let kvs = Json.dict_of_rpc ~rpc in
	let description = Json.assoc_opt ~key:"description" ~default:"" kvs in
	let units = Json.assoc_opt ~key:"units" ~default:"" kvs in
	let ty =
		Json.ds_ty_of_string (Json.assoc_opt ~key:"type" ~default:"absolute" kvs)
	in
	let value =
		value_of_string (Rpc.string_of_rpc (List.assoc "value_type" kvs)) in
	let min =
		float_of_string (Json.assoc_opt ~key:"min" ~default:"-infinity" kvs) in
	let max =
		float_of_string (Json.assoc_opt ~key:"max" ~default:"infinity" kvs) in
	let owner =
		Json.owner_of_string (Json.assoc_opt ~key:"owner" ~default:"host" kvs) in
	let ds = Ds.ds_make ~name ~description ~units
		~ty ~value ~min ~max ~default:true () in
	ds, owner

let parse_metadata metadata =
	try
		let open Rpc in
		let rpc = Jsonrpc.of_string metadata in
		let kvs = Json.dict_of_rpc ~rpc in
		let datasource_rpcs = Json.dict_of_rpc (List.assoc "datasources" kvs) in
		List.map uninitialised_ds_of_rpc datasource_rpcs
	with _ -> raise Invalid_payload

let read_payload cs =
	(* Check the header string is present and correct. *)
	let header = Read.header cs in
	if not (header = default_header) then
		raise Invalid_header_string;
	(* Check that the data checksum has changed. Since the checksummed data
	 * includes the timestamp, this should change with every update. *)
	let data_checksum = Read.data_checksum cs in
	if data_checksum = !last_data_checksum then raise No_update;
	let metadata_checksum = Read.metadata_checksum cs in
	let datasource_count = Read.datasource_count cs in
	let timestamp = Read.timestamp cs in
	(* Check the data checksum is correct. *)
	let data_checksum_calculated =
		Cstruct_hash.md5sum cs
			timestamp_start
			(timestamp_bytes + datasource_count * datasource_value_bytes)
	in
	if not (data_checksum = data_checksum_calculated)
	then raise Invalid_checksum
	else last_data_checksum := data_checksum;
	(* Read the datasource values. *)
	let datasources =
		if metadata_checksum = !last_metadata_checksum then begin
			(* Metadata hasn't changed, so just read the datasources values. *)
			Read.datasource_values cs !cached_datasources
		end else begin
			(* Metadata has changed - we need to read it to find the types of the
			 * datasources, then go back and read the values themselves. *)
			let metadata_length = Read.metadata_length cs datasource_count in
			let metadata = Read.metadata cs datasource_count metadata_length in
			(* Check the metadata checksum is correct. *)
			if not (metadata_checksum = (Digest.string metadata))
			then raise Invalid_checksum;
			(* If all is OK, cache the metadata checksum and read the values
			 * based on this new metadata. *)
			last_metadata_checksum := metadata_checksum;
			Read.datasource_values cs (parse_metadata metadata)
		end
	in
	cached_datasources := datasources;
	{
		timestamp = timestamp;
		datasources = datasources;
	}

let write_payload alloc_cstruct payload =
	let metadata = Rrdp.json_metadata_of_dss payload.datasources in
	let datasource_count = List.length payload.datasources in
	let metadata_length = String.length metadata in
	let total_bytes = get_total_bytes datasource_count metadata_length in
	let cs = alloc_cstruct total_bytes in
	(* Write header. *)
	Write.header cs;
	(* Write metadata checksum. *)
	Write.metadata_checksum cs (Digest.string metadata);
	(* Write number of datasources. *)
	Write.datasource_count cs datasource_count;
	(* Write timestamp. *)
	Write.timestamp cs (now ());
	(* Write datasource values. *)
	Write.datasource_values cs
		(List.map (fun (ds, _) -> ds.Ds.ds_value) payload.datasources);
	(* Write the metadata. *)
	Write.metadata cs metadata datasource_count;
	(* Write the metadata length. *)
	Write.metadata_length cs metadata_length datasource_count;
	(* Write the data checksum. *)
	let data_checksum =
		Cstruct_hash.md5sum cs timestamp_start
			(timestamp_bytes + datasource_count * datasource_value_bytes) in
	Write.data_checksum cs data_checksum
