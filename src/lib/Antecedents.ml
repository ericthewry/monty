open Core

let rec get_antecedents_tree (node : Grammar.t) : Grammar.t list =
  match node with
  | Item _ -> [node]
  | Node {label=_; left; right} ->
    let f = Option.value_map ~f:get_antecedents_tree ~default:[] in
    [node]
    @ f left
    @ f right

let get_antecedents_phrase ({np; vp} : Grammar.s) : Grammar.t list =
  get_antecedents_tree np
  @ get_antecedents_tree vp

let get_antecedents (discourse : Grammar.d) : Grammar.t list  =
  List.bind discourse ~f:get_antecedents_phrase
