
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

let rec read_attrs str acc = 
  let name = Ba_utils.st_get_uleb str in
  let form = Ba_utils.st_get_uleb str in

  let attr = {
    name  = name;
    form  = form;
  } in

  if (attr.name != 0) || (attr.form != 0) then
    read_attrs str (attr::acc)
  else
    acc


let rec _read_abbrev str idx acc = 
  let code = Ba_utils.st_get_uleb str in
  let tag  = Ba_utils.st_get_uleb str in

  if code != 0 then begin
    let child = Ba_utils.st_get_uleb str in

    let abbrev = {
      idx   = idx;
      tag   = tag;
      child = child;
      attrs = read_attrs str [];
    } in

    _read_abbrev str (idx+1) (abbrev::acc)
  end else
    acc

  
let read_abbrev mm idx = 
  let sread = {
    mm = mm;
    ptr = idx;
  } in
  List.rev (_read_abbrev sread 1 [])


let rec dump_abbrev_attribs lst = 
  List.iter (fun a -> Printf.printf "  name = 0x%2x form = 0x%2x\n" a.name a.form) lst


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

let dump_info mm idx = 
  let cu_len  = get_uint32 mm idx in
  let cu_ver  = get_uint16 mm (idx+4) in
  let cu_ao   = get_uint32 mm (idx+6) in
  let cu_bits = get_byte mm (idx+10) in
  Printf.printf "cu: len=%x ver=%d off=%d bits=%d\n" cu_len cu_ver cu_ao (int_of_char cu_bits);
  Printf.printf "idx=%x\n" idx;
  
  dump_hex mm idx 50;
