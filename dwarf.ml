
open Ba_utils


type abbrev_attrib = {
  name : int;
  form : int;
}

type abbrev = {
  idx   : int;
  tag   : int;
  child : int;
  attrs : abbrev_attrib list;
}

type form_nam = FORM_addr | FORM_block2 | FORM_block4 | FORM_data2 | FORM_data4 | FORM_data8 | FORM_string
              | FORM_block | FORM_block1 | FORM_data1 | FORM_flag | FORM_sdata | FORM_strp | FORM_udata
              | FORM_ref_addr | FORM_ref1 | FORM_ref2 | FORM_ref4 |  FORM_ref8 | FORM_ref_udata | FORM_indirect
              | FORM_sec_offset | FORM_exprloc | FORM_flag_present | FORM_ref_sig8
let form_names = [
  (0x01, "addr",         FORM_addr);
  (0x03, "block2",       FORM_block2);
  (0x04, "block4",       FORM_block4);
  (0x05, "data2",        FORM_data2);
  (0x06, "data4",        FORM_data4);
  (0x07, "data8",        FORM_data8);
  (0x08, "string",       FORM_string);
  (0x09, "block",        FORM_block);
  (0x0a, "block1",       FORM_block1);
  (0x0b, "data1",        FORM_data1);
  (0x0c, "flag",         FORM_flag);
  (0x0d, "sdata",        FORM_sdata);
  (0x0e, "strp",         FORM_strp);
  (0x0f, "udata",        FORM_udata);
  (0x10, "ref_addr",     FORM_ref_addr);
  (0x11, "ref1",         FORM_ref1);
  (0x12, "ref2",         FORM_ref2);
  (0x13, "ref4",         FORM_ref4);
  (0x14, "ref8",         FORM_ref8);
  (0x15, "ref_udata",    FORM_ref_udata);
  (0x16, "indirect",     FORM_indirect);
  (0x17, "sec_offset",   FORM_sec_offset);
  (0x18, "exprloc",      FORM_exprloc);
  (0x19, "flag_present", FORM_flag_present);
  (0x20, "ref_sig8",     FORM_ref_sig8);
]


let form_get_name id =
  List.fold_left  (fun acc (_id, _name, _) -> if _id = id then _name else acc ) "?" form_names

let rec read_attrs stream acc =
  let name = Ba_utils.st_get_uleb stream in
  let form = Ba_utils.st_get_uleb stream in

  let attr = {
    name  = name;
    form  = form;
  } in

  if (attr.name != 0) || (attr.form != 0) then
    read_attrs stream (attr::acc)
  else
    acc


let rec _read_abbrev stream idx acc =
  let code = Ba_utils.st_get_uleb stream in
  let tag  = Ba_utils.st_get_uleb stream in

  if code != 0 then begin
    let child = Ba_utils.st_get_uleb stream in

    let abbrev = {
      idx   = idx;
      tag   = tag;
      child = child;
      attrs = List.rev (read_attrs stream []);
    } in

    _read_abbrev stream (idx+1) (abbrev::acc)
  end else
    acc

  
let read_abbrev mm idx = 
  let stream = {
    mm = mm;
    ptr = idx;
  } in
  List.rev (_read_abbrev stream 1 [])


let rec dump_abbrev_attribs lst = 
  List.iter (fun a -> Printf.printf "  name = 0x%2x form = 0x%02x (%s)\n" a.name a.form (form_get_name a.form)) lst


let rec dump_abbrev lst = 
  List.iter (fun a ->
              Printf.printf "idx = %3d tag = 0x%x\n" a.idx a.tag;
              dump_abbrev_attribs a.attrs;
            ) lst


let rec _dump_hex mm idx len =
  Printf.printf "%02x " (int_of_char (get_byte mm (idx)));
  if len == 0 then
    ()
  else
    _dump_hex mm (idx+1) (len-1)


let dump_hex mm idx len =
  _dump_hex mm idx len;
  Printf.printf "\n"

let read_die_attr stream attr =
  Printf.printf "  read die form= 0x%x\n" attr.form;
  match attr.form with
  | 0x0e -> Ba_utils.st_skip stream 4
  | 0x17 -> Ba_utils.st_skip stream 4
  | 0x05 -> Ba_utils.st_skip stream 2
  | 0x01 -> Ba_utils.st_skip stream 8
  | 0x0b -> Ba_utils.st_skip stream 1
  | 0x13 -> Ba_utils.st_skip stream 4
  | 0x18 -> let s = Ba_utils.st_get_uleb stream in Ba_utils.st_skip stream s
  | 0x0d -> Ba_utils.st_skip stream 1
  | 0x19 -> ()
  | _  -> Printf.printf "unknown attr.form %x\n" attr.form

let rec read_die_attrs stream attrs =
  match attrs with
  | [] -> ()
  | hd :: tl ->
    read_die_attr stream hd;
    read_die_attrs stream tl


let find_abbrev_by_id abbrevs id =
  let empty = {idx = 0; tag = 0; child = 0; attrs = [];} in
  List.fold_left (fun acc x -> if x.idx = id then x else acc ) empty abbrevs

let rec read_die stream abbrevs =
  let abbr_id = Ba_utils.st_get_uleb stream in
  Printf.printf "abbrev num: %d\n" abbr_id;
  read_die_attrs stream (find_abbrev_by_id abbrevs abbr_id).attrs;
  read_die stream abbrevs


let dump_info mm idx abbrevs =
  let cu_len  = get_uint32 mm idx in
  let cu_ver  = get_uint16 mm (idx+4) in
  let cu_ao   = get_uint32 mm (idx+6) in
  let cu_bits = get_byte mm (idx+10) in
  Printf.printf "cu: len=%x ver=%d off=%d bits=%d\n" cu_len cu_ver cu_ao (int_of_char cu_bits);
  Printf.printf "idx=%x\n" idx;
  
  dump_hex mm idx 50;
  let stream = {
    mm = mm;
    ptr = idx+11;
  } in
  read_die stream abbrevs
