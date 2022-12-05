include Serialiser_intf.Intf

type json_diff_simple =
  | Add_field of string * Yojson.Basic.t
  | Remove_field of string * Yojson.Basic.t
  | Update_field of string * Yojson.Basic.t
  | List of json_diff_simple list
  | Nest of string * json_diff
  | No_diff of string

and json_diff = json_diff_simple

module Json : S with type t = Yojson.Basic.t and type Diff.t = json_diff
