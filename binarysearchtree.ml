type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

let empty_tree : 'a tree = Empty

let rec insert (value : 'a) (tree : 'a tree) : 'a tree =
  match tree with
  | Empty -> Node (Empty, value, Empty)
  | Node (left, root_value, right) ->
      if value < root_value then
        Node (insert value left, root_value, right)
      else if value > root_value then
        Node (left, root_value, insert value right)
      else
        tree

let rec search (value : 'a) (tree : 'a tree) : bool =
  match tree with
  | Empty -> false
  | Node (left, root_value, right) ->
      if value = root_value then
        true
      else if value < root_value then
        search value left
      else
        search value right

let rec remove (value : 'a) (tree : 'a tree) : 'a tree =
  match tree with
  | Empty -> Empty
  | Node (left, root_value, right) ->
      if value < root_value then
        Node (remove value left, root_value, right)
      else if value > root_value then
        Node (left, root_value, remove value right)
      else
        match (left, right) with
        | (Empty, _) -> right
        | (_, Empty) -> left
        | (Node (_, _, _), Node (_, _, _)) ->
            let successor = find_successor right in
            Node (left, successor, remove successor right)

and find_successor (tree : 'a tree) : 'a =
  match tree with
  | Empty -> failwith "Error: Empty tree"
  | Node (left, root_value, _) ->
      if left = Empty then
        root_value
      else
        find_successor left
