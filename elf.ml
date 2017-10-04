
open Ba_utils


type section = {
  idx            : int;
  link           : int;
  mutable name   : string;
  offset         : int;
  name_offset    : int;
  entsize        : int
};;


let rec _read_zt_str mm idx buf =
  let chr = get_byte mm idx in
  if Char.code chr != 0 then begin
    Buffer.add_char buf chr;
    _read_zt_str mm (idx+1) buf
  end


let read_zt_string mm idx =
  let buf = Buffer.create 20 in
  _read_zt_str mm idx buf;
  Bytes.to_string (Buffer.to_bytes buf)


let rec read_sections mm ptr es num idx lst =
  if num == 0 then
    lst
  else begin
    (*let sh_type  = (get_uint16 mm (ptr + 0x04)) in*)

    let sec = {
      idx = idx;
      link =        (get_uint32 mm (ptr + 0x28));
      name = "?";
      name_offset = (get_uint32 mm (ptr + 0x00));
      offset      = (get_uint64 mm (ptr + 0x18));
      entsize     = (get_uint64 mm (ptr + 0x38));
    } in
    read_sections mm (ptr+es) es (num-1) (idx+1) (sec::lst);
  end

let section_find_off sections id =
  List.fold_left (fun acc x -> if x.idx == id then x.offset else acc) 0 sections

let section_find_off_by_name sections name =
  List.fold_left (fun acc x -> if x.name = name then x.offset else acc) 0 sections

let decode_names mm sections =
  let sidx = (get_uint16 mm 0x3e) in
  let soff = (section_find_off sections sidx) in
  List.iter (fun x -> x.name <- (read_zt_string mm (soff+x.name_offset)) ) sections


let dump_sections sections =
  List.iter (fun x ->
    Printf.printf "idx = %02d name=%-16s link=%2d offset=%8d entsize=%d\n" x.idx x.name x.link x.offset x.entsize
  ) sections

let dump_info mm idx = 
  let cu_len  = get_uint32 mm idx in
  let cu_ver  = get_uint16 mm (idx+4) in
  let cu_ao   = get_uint32 mm (idx+6) in
  let cu_bits = get_byte mm (idx+10) in
  Printf.printf "cu: len=%x ver=%d off=%d bits=%d\n" cu_len cu_ver cu_ao (int_of_char cu_bits);
  Printf.printf "idx=%x\n" idx;
  (*
  Printf.printf "%x %x %x %x %x %x %x %x %x %x %x %x %x\n" (int_of_char (get_byte mm idx))
                                 (int_of_char (get_byte mm (idx+1)))
                                 (int_of_char (get_byte mm (idx+2)))
                                 (int_of_char (get_byte mm (idx+3)))
                                 (int_of_char (get_byte mm (idx+4)))
                                 (int_of_char (get_byte mm (idx+5)))
                                 (int_of_char (get_byte mm (idx+6)))
                                 (int_of_char (get_byte mm (idx+7)))
                                 (int_of_char (get_byte mm (idx+8)))
                                 (int_of_char (get_byte mm (idx+9)))
                                 (int_of_char (get_byte mm (idx+0xa)))
                                 (int_of_char (get_byte mm (idx+0xb)))
                                 (int_of_char (get_byte mm (idx+0xc)))
*)

let rec read_child_str str = 
  let name    = Ba_utils.st_get_uleb str in
  let form    = Ba_utils.st_get_uleb str in
  Printf.printf "  name=%x form=%x\n" name form;
  if (name != 0) || (form != 0) then read_child_str str


let rec _dump_abb str idx = 
  let code      = Ba_utils.st_get_uleb str in
  let tag       = Ba_utils.st_get_uleb str in
  let has_child = Ba_utils.st_get_uleb str in
  Printf.printf "%d code=%x tag_id=%x child=%x\n" idx code tag has_child;
  read_child_str str;
  if tag != 0 then _dump_abb str (idx+1)
  

let dump_abb mm idx = 
  let sread = {
    Ba_utils.mm = mm;
    Ba_utils.ptr = idx;
  } in
  _dump_abb sread 0


let() = 
        let fname      = "b" in
        let fd         = Unix.openfile fname [Unix.O_RDONLY] 0 in
        let image_size = (Unix.stat fname).Unix.st_size in
        Printf.printf "image_size=%d\n" image_size;
        let mm         = Bigarray.Genarray.map_file fd Bigarray.Char Bigarray.C_layout false (Array.of_list [image_size]) in
        let cls        = (int_of_char(get_byte mm 4)) in
        let sh_off       =  (get_uint64 mm 0x28) in
        let sh_ent_size  =  (get_uint16 mm 0x3a) in
        let sh_ent_num   =  (get_uint16 mm 0x3c) in

        Printf.printf "magic= %x\n" (get_uint32 mm 0);
        Printf.printf "class = %d\n" cls;
        Printf.printf "endian= %d\n" (int_of_char(get_byte mm 5));
        Printf.printf "program_header_idx = %d\n" (get_uint64 mm 0x20);
        Printf.printf "section_header_off = %d\n" sh_off;
        Printf.printf "section_header_es  = %d\n" sh_ent_size;
        Printf.printf "section_header_num = %d\n" sh_ent_num;
        let sections = List.rev (read_sections mm sh_off sh_ent_size sh_ent_num 0 []) in
        decode_names mm sections;
        dump_sections sections;
        dump_abb  mm (section_find_off_by_name sections ".debug_abbrev");
        dump_info mm (section_find_off_by_name sections ".debug_info");
        print_string "aa\n";;
