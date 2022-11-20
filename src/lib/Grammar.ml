open Core

type t =
  | Item of {label : string ; item : string }
  | Node of {label: string; left: t option; right: t option}
  [@@deriving sexp, equal]

let tick = Printf.sprintf "%s'"

let xbar ?(spec = None) ?(comp = None) ?(adjunct = None) ~head label =
  let base_label = label in
  let x = Item {label; item = head} in
  let label = tick label in
  let x' = Node {label; left = Some x; right = comp} in
  let x' = Node {label; left = Some x'; right = adjunct } in
  Node {
    label = Printf.sprintf "%sP" base_label;
    left  = spec;
    right = Some x'}


let pretty_print_item label item =
  if String.is_empty label then
    item
  else
    Printf.sprintf "[%s]%s" item label


let pretty_print_single ~to_string label = function
  | Item {label=_; item} ->
    pretty_print_item label item
  | Node n ->
    to_string @@ Node {n with label}

let rec to_string = function
  | Item {label; item} ->
    pretty_print_item label item
  | Node {label; left; right} ->
    match left, right with
    | None, None ->
      failwithf "Cannot have both arguments of label:%s be None" label ()
    | Some left, None ->
      pretty_print_single ~to_string label left
    | None, Some right ->
        pretty_print_single ~to_string label right
    | Some left, Some right ->
      Printf.sprintf "[%s %s]%s" (to_string left) (to_string right) label

let vp ?comp ?adjunct head =
  xbar "V" ~spec:None ~adjunct ~comp ~head

let has_label queried_label = function
  | Item {label; item=_}
  | Node {label; left=_; right=_} ->
    String.(label = queried_label)

let vp_e =
  Node {
    label = "VP";
    left = Some (Item {label = ""; item = "E" });
    right = Some (Item {label = ""; item = "?"})
  }

let np ?comp ?adjunct head =
  xbar "N" ~spec:None ~comp ~adjunct ~head

let advp ?comp ?adjunct head =
  xbar "A" ~spec:None ~comp ~adjunct ~head

type s = {np : t; vp : t} [@@deriving sexp, equal]

let phrase np vp : s = {np; vp}

let phrase_to_string {np; vp} =
  Printf.sprintf
    "[%s %s]S" (to_string np) (to_string vp)

type d = s list [@@deriving sexp, equal]
