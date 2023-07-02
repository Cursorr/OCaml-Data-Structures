type color = Red | Black

type 'a rbtree =
  | Empty
  | Node of color * 'a * 'a rbtree * 'a rbtree

let empty_tree : 'a rbtree = Empty

let rec is_empty (tree : 'a rbtree) : bool =
  match tree with
  | Empty -> true
  | _ -> false

let rec insert (value : 'a) (tree : 'a rbtree) : 'a rbtree =
  let rec insert_helper (t : 'a rbtree) : 'a rbtree =
    match t with
    | Empty -> Node (Red, value, Empty, Empty)
    | Node (color, v, left, right) ->
      if value < v then
        balance color v (insert_helper left) right
      else if value > v then
        balance color v left (insert_helper right)
      else
        t
  in
  match insert_helper tree with
  | Node (_, v, left, right) -> Node (Black, v, left, right)
  | Empty -> Empty

and balance (pcolor : color) (pvalue : 'a) (left : 'a rbtree) (right : 'a rbtree) : 'a rbtree =
  match (pcolor, pvalue, left, right) with
  | (Black, pv, Node (Red, lv, Node (Red, llv, lll, llr), lr), r)
  | (Black, pv, Node (Red, lv, ll, Node (Red, llv, lrl, lrr)), r)
  | (Black, pv, l, Node (Red, rv, Node (Red, rlv, rll, rlr), rr))
  | (Black, pv, l, Node (Red, rv, rl, Node (Red, rlv, rrl, rrr))) ->
    Node (Red, pv, Node (Black, lv, ll, lr), Node (Black, rv, rl, rr))
  | _ -> Node (pcolor, pvalue, left, right)

let rec search (value : 'a) (tree : 'a rbtree) : bool =
  match tree with
  | Empty -> false
  | Node (_, v, left, right) ->
    if value < v then
      search value left
    else if value > v then
      search value right
    else
      true
