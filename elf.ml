
open Ba_utils


type section = {
  idx          : int;
  link         : int;
  offset       : int;
  name_offset  : int;
  stype        : int;
  entsize      : int;
  size         : int;
  mutable name : string;
}

type info = {
  cls      : int;
  endian   : int;
  sections : section list;
}

let rec _read_sections mm ptr element_size num_of_elements idx lst =
  let section = {
      idx         = idx;
      name_offset = get_uint32 mm (ptr + 0x00);
      stype       = get_uint16 mm (ptr + 0x04);
      link        = get_uint32 mm (ptr + 0x28);
      offset      = get_uint64 mm (ptr + 0x18);
      entsize     = get_uint64 mm (ptr + 0x38);
      size        = get_uint64 mm (ptr + 0x20);
      name        = "?";
  } in
  let new_lst = section::lst in
  match num_of_elements with
    | 1 -> new_lst
    | _ -> _read_sections mm (ptr+element_size) element_size (num_of_elements-1) (idx+1) new_lst

let read_sections_ mm ptr es num = _read_sections mm ptr es num 0 []

let section_find sections comparator =
  List.fold_left (fun acc x -> if (comparator x) then x.offset else acc) 0 sections

let section_find_offset_by_id   sections id   = section_find sections (fun x-> x.idx = id)
let section_find_offset_by_name sections name = section_find sections (fun x-> x.name = name)

let decode_section_names mm sections =
  let index_of_names_section  = get_uint16 mm 0x3e in
  let offset_of_names_section = section_find_offset_by_id sections index_of_names_section in
  List.iter ( fun section -> section.name <- read_zt_string mm (offset_of_names_section + section.name_offset) ) sections

let dump_sections sections =
  List.iter (fun x ->
    Printf.printf "idx = %02d name=%-16s link=%2d offset=%8d entsize=%d size=%d\n" x.idx x.name x.link x.offset x.entsize x.size
  ) sections


let read_info mm = 
  let sh_off       =  get_uint64 mm 0x28 in
  let sh_ent_size  =  get_uint16 mm 0x3a in
  let sh_ent_num   =  get_uint16 mm 0x3c in

  let info = {
    cls      = int_of_char(get_byte mm 4);
    endian   = int_of_char(get_byte mm 5);
    sections = List.rev (read_sections_ mm sh_off sh_ent_size sh_ent_num);
  } in
  decode_section_names mm info.sections;
  info

