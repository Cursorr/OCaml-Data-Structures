type 'a stack = {
  mutable elements: 'a list;
}

let create_stack () : 'a stack =
  { elements = [] }

let is_empty (stack : 'a stack) : bool =
  match stack.elements with
  | [] -> true
  | _ -> false

let push (value : 'a) (stack : 'a stack) : unit =
  stack.elements <- value :: stack.elements

let pop (stack : 'a stack) : 'a option =
  match stack.elements with
  | [] -> None
  | hd :: tl ->
    stack.elements <- tl;
    Some hd

let peek (stack : 'a stack) : 'a option =
  match stack.elements with
  | [] -> None
  | hd :: _ -> Some hd
