




let mmap_file name = 
  let fd         = Unix.openfile name [Unix.O_RDONLY] 0 in
  let image_size = (Unix.stat name).Unix.st_size in
  Printf.printf "image_size=%d\n" image_size;
  (fd, Bigarray.Genarray.map_file fd Bigarray.Char Bigarray.C_layout false (Array.of_list [image_size]))


let() = 
  let (fd, mm) = mmap_file "b" in

  let elf_info = Elf.read_info mm in
  Elf.dump_sections elf_info.Elf.sections;

  let dwarf_abbrev = Dwarf.read_abbrev mm (Elf.section_find_offset_by_name elf_info.Elf.sections ".debug_abbrev") in
  Dwarf.dump_abbrev dwarf_abbrev;
  Printf.printf "---\n";


  (*Dwarf.dump_info mm (Elf.section_find_offset_by_name elf_info.Elf.sections ".debug_info") dwarf_abbrev;*)
  Unix.close fd

