open Core

type t =
  | Symbol of string
  | App of t list
  | Abs of string * t
  [@@deriving compare, eq]

let symb x = Symbol x
let app es = match es with
  | [] -> failwith "cannot apply 0 elements"
  | [e] -> e
  | _ -> App es

let abs x e =
  Abs (x, e)

let rec to_string e =
  match e with
  | Symbol x -> x
  | App es ->
    List.map ~f:to_string es
    |> String.concat ~sep:" "
    |> Printf.sprintf "(%s)"
  | Abs (x,e) ->
    Printf.sprintf "(\\%s. %s)" x (to_string e)

let rec subst param arg body =
  match body with
  | Abs (x, body) when String.(x <> param) ->
    abs x @@ subst param arg body
  | Abs (_, _) ->
    body
  | App es ->
    app @@ List.map es ~f:(subst param arg)
  | Symbol x when String.(x = param) ->
    arg
  | Symbol _ ->
    body

let rec beta e =
  match e with
  | Symbol x -> symb x
  | Abs (x, t) -> abs x t
  | App [] ->
    failwith "cannot have 0 terms in application"
  | App (f::args) ->
    let args = List.map ~f:beta args in
    match beta f, args with
    | Symbol x, _ ->
      app @@ Symbol x :: args
    | App args', _->
      beta @@ app @@ args' @ args
    | Abs (x, e), (arg0::args) ->
      beta @@ app @@ subst x arg0 e :: args
    | _ ->
      failwithf "ill formed application: %s" (to_string e) ()

let lifting gen e =
  let max_data m =
    String.Map.data m
    |> List.max_elt ~compare:Int.compare
    |> Option.value ~default:0
  in
  let rec loop bound gen e =
    match e with
    | Symbol x when String.Set.exists bound ~f:(String.equal x) ->
      (gen, [], e)
    | Symbol x ->
      let idx, gen =
        match String.Map.find gen x with
        | Some idx -> (idx, gen)
        | None ->
          let idx = 1 + max_data gen in
          (idx, String.Map.set gen ~key:x ~data:idx)
      in
      let idx = symb @@ Int.to_string idx in
      (gen, [app [symb x; idx]], idx)
    | App (Symbol f::es) ->
      let gen, lifts, es =
        List.fold es ~init:(gen,[],[])
          ~f:(fun (gen, lifts, lifted) e ->
              let gen, lifts', e = loop bound gen e in
              (gen, lifts @ lifts', lifted@[e])
            ) in
      (gen, lifts, app @@ symb f :: es)
    | App es ->
      let gen, lifts, es =
        List.fold es ~init:(gen,[],[])
          ~f:(fun (gen, lifts, lifted) e ->
              let gen, lifts', e = loop bound gen e in
              (gen, lifts @ lifts', lifted@[e])
            ) in
      (gen, lifts, app es)
    | Abs (x,e) ->
      let bound = String.Set.add bound x in
      let gen, lifts, e = loop bound gen e in
      (gen, lifts, abs x e)
  in
  loop String.Set.empty gen e
