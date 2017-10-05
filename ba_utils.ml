
(* some utilities to work with Bigarray *)


type stream_reader = {
  mm: (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Genarray.t;
  mutable ptr: int;
};;


let st_get_byte sr =
  sr.ptr <- sr.ptr + 1;
  int_of_char (Bigarray.Genarray.get sr.mm [|sr.ptr-1|])

let rec _st_get_uleb sr acc bit =
  let b = st_get_byte sr in
  let now = acc lor (b lsl bit) in
  if (b land 0x80) == 0 then
    now
  else
    _st_get_uleb sr now (bit+7)

let st_get_uleb sr =
  _st_get_uleb sr 0 0

let get_byte mm ptr =
  Bigarray.Genarray.get mm [|ptr|]

let rec dec_int mm ptr togo bits acu = 
  if togo == 0 then acu
else
  let acx = ((int_of_char (get_byte mm (ptr)) ) lsl bits ) lor acu in
  (dec_int mm (ptr+1) (togo-1) (bits+8) acx)


(* fake 64bit support *)
let rec dec_int64 mm ptr togo bits acu = 
  if togo == 0 then acu
else
  let acx = ((int_of_char (get_byte mm (ptr)) ) lsl bits ) lor acu in
  (dec_int64 mm (ptr+1) (togo-1) (bits+8) acx)

let get_uint32 mm ptr =
  dec_int mm ptr 4 0 0

let get_uint16 mm ptr =
  dec_int mm ptr 2 0 0

let get_uint64 mm ptr =
  dec_int64 mm ptr 8 0 0


let get_bytes mm ptr len =
  let out = Bytes.create len in
  for idx = 0 to (len-1) do
    Bytes.set out idx (get_byte mm (ptr+idx));
  done;
  out

(* read zero terminated string *)
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
