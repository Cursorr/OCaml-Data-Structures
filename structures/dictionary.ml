type ('a, 'b) dictionary =
  | Empty
  | Node of ('a * 'b) * ('a, 'b) dictionary * ('a, 'b) dictionary

let empty_dictionary : ('a, 'b) dictionary = Empty

let rec add_entry (key : 'a) (value : 'b) (dict : ('a, 'b) dictionary) : ('a, 'b) dictionary =
  match dict with
  | Empty -> Node ((key, value), Empty, Empty)
  | Node ((k, v), left, right) ->
      if key < k then
        Node ((k, v), add_entry key value left, right)
      else if key > k then
        Node ((k, v), left, add_entry key value right)
      else
        Node ((key, value), left, right)

let rec find_value (key : 'a) (dict : ('a, 'b) dictionary) : 'b option =
  match dict with
  | Empty -> None
  | Node ((k, v), left, right) ->
      if key = k then
        Some v
      else if key < k then
        find_value key left
      else
        find_value key right

let rec remove_entry (key : 'a) (dict : ('a, 'b) dictionary) : ('a, 'b) dictionary =
  match dict with
  | Empty -> Empty
  | Node ((k, v), left, right) ->
      if key < k then
        Node ((k, v), remove_entry key left, right)
      else if key > k then
        Node ((k, v), left, remove_entry key right)
      else
        match (left, right) with
        | (Empty, _) -> right
        | (_, Empty) -> left
        | (Node (_, _, _), Node (_, _, _)) ->
            let successor = find_min right in
            Node (successor, left, remove_entry (fst successor) right)

and find_min (tree : ('a, 'b) dictionary) : ('a * 'b) =
  match tree with
  | Empty -> failwith "Error: Empty tree"
  | Node (kv, Empty, _) -> kv
  | Node (_, left, _) -> find_min left
