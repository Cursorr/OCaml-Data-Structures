type 'a node =
  {
    value: 'a;
    mutable next: 'a node option;
  }

type 'a linked_list =
  {
    mutable head: 'a node option;
    mutable tail: 'a node option;
  }

let create_linked_list () : 'a linked_list =
  {
    head = None;
    tail = None;
  }

let is_empty (list : 'a linked_list) : bool =
  match list.head with
  | None -> true
  | _ -> false

let prepend (value : 'a) (list : 'a linked_list) : unit =
  let new_node = { value; next = list.head } in
  list.head <- Some new_node;
  if list.tail = None then
    list.tail <- Some new_node

let append (value : 'a) (list : 'a linked_list) : unit =
  let new_node = { value; next = None } in
  match list.tail with
  | None ->
    list.head <- Some new_node;
    list.tail <- Some new_node
  | Some tail_node ->
    tail_node.next <- Some new_node;
    list.tail <- Some new_node

let rec find (value : 'a) (list : 'a linked_list) : bool =
  let rec find_helper (node : 'a node option) : bool =
    match node with
    | None -> false
    | Some curr_node ->
      if curr_node.value = value then
        true
      else
        find_helper curr_node.next
  in
  find_helper list.head

let rec remove (value : 'a) (list : 'a linked_list) : unit =
  let rec remove_helper (prev : 'a node option) (node : 'a node option) : unit =
    match node with
    | None -> ()
    | Some curr_node ->
      if curr_node.value = value then (
        match prev with
        | None -> list.head <- curr_node.next
        | Some prev_node -> prev_node.next <- curr_node.next
      )
      else
        remove_helper node (curr_node.next)
  in
  remove_helper None list.head

let to_list (list : 'a linked_list) : 'a list =
  let rec to_list_helper (node : 'a node option) (acc : 'a list) : 'a list =
    match node with
    | None -> acc
    | Some curr_node -> to_list_helper curr_node.next (curr_node.value :: acc)
  in
  List.rev (to_list_helper list.head [])

