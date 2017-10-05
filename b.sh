set -x
ocamlopt -g -c ba_utils.ml
ocamlopt -g -c elf.ml 
ocamlopt -g -c dwarf.ml 
ocamlopt -g -c main.ml 
ocamlopt -g -o a.out  bigarray.cmxa unix.cmxa ba_utils.cmx elf.cmx dwarf.cmx main.cmx

