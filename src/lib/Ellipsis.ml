open Core

let vp_ellipsis_resolution discourse utterance : Grammar.s list =
  let open Grammar in
  let open List.Let_syntax in
  let rec loop utterance =
    match utterance with
    | Item _ -> [utterance]
    | Node {label; left; right} ->
      let open Grammar in
      if equal utterance vp_e then
        let%bind antecedent = Antecedents.get_antecedents discourse in
        if has_label "VP" antecedent then
          Node {
            label = "VP";
            left = Some (Item {label = ""; item = "E"});
            right = Some antecedent
          } |> return
        else
          []
      else
        match left, right with
        | None, None -> failwithf "label %s cannot take 0 arguments" label ()
        | Some left, Some right ->
          let%bind left = loop left in
          let%map right = loop right in
          Node {label;
                left = Some left;
                right = Some right;
               }
        | Some left, _ ->
          let%map left = loop left in
          Node {label;
                left = Some left;
                right = None
               }
        | _, Some right ->
          let%map right = loop right in
          Node {label;
                left = None;
                right = Some right
               }
  in
  let%map vp = loop utterance.vp in
  {utterance with vp}
