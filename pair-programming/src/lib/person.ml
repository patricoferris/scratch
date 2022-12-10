module Name = struct
  type t = {
    name : string;
    nickname : string option;
  }

  let v ?nick name = { name; nickname = nick }

  let name t = t.name
  let nick t = t.nickname
end

type t = { fullname : Name.t; age : int }

let v ?nick name age =
  let fullname = Name.v ?nick name in
  { fullname; age }

let age t = t.age
let name t = t.fullname

let pp ppf t =
  Format.fprintf ppf "name: %s, age: %i\n" t.fullname.name t.age


module type Serial = sig
  type t

  val serialise : t -> string
end

module Disk (S : Serial) = struct
  let write_to_file file t =
    Out_channel.with_open_bin file 
      (fun oc -> Out_channel.output_string oc (S.serialise t))
end

module Person_serialise = struct
  type nonrec t = t

  let serialise t =
    Format.fprintf Format.str_formatter "%a" pp t;
    Format.flush_str_formatter ()
end

module File = Disk (Person_serialise)

let to_file f t = File.write_to_file f t