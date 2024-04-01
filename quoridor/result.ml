(*(* This file defines a result monad *)

  type ('a, 'e) t = ('a, 'e) result

  let return (a : 'a) : ('a, 'e) t = Ok a

  let bind (m : ('a, 'e) t) (f : 'a -> ('b, 'e) t) =
    match m with Ok a -> f a | Error e -> Error e

  let fail (e : 'e) : ('a, 'e) t = Error e

  let one_of (r1 : ('a, 'e) t) (r2 : ('a, 'e) t) (f : 'e -> 'e -> 'e) =
    match r1 with
    | Ok _ -> r1
    | Error e1 -> begin
        match r2 with Ok _ -> r2 | Error e2 -> fail @@ f e1 e2
      end

  let is_ok (r : ('a, 'e) t) : bool =
    match r with Ok _ -> true | Error _ -> false

  let map_error (f : 'e1 -> 'e2) (m : ('a, 'e1) t) : ('a, 'e2) t =
    match (m : ('a, 'e1) t) with Ok a -> Ok a | Error e -> Error (f e)

  module Syntax = struct
    let ( let* ) = bind
    let ( let+ ) (m : ('a, 'e) t) (f : 'a -> 'b) : ('b, 'e) t = return (bind m f)
  end*)
