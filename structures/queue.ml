type 'a queue = {
  mutable front: 'a list;
  mutable rear: 'a list;
}

let create_queue () : 'a queue =
  { front = []; rear = [] }

let is_empty (queue : 'a queue) : bool =
  match queue.front, queue.rear with
  | [], [] -> true
  | _, _ -> false

let enqueue (value : 'a) (queue : 'a queue) : unit =
  queue.rear <- value :: queue.rear

let dequeue (queue : 'a queue) : 'a option =
  match queue.front with
  | [] ->
    begin
      match List.rev queue.rear with
      | [] -> None
      | hd :: tl ->
        queue.front <- tl;
        queue.rear <- [];
        Some hd
    end
  | hd :: tl ->
    queue.front <- tl;
    Some hd
