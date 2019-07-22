let%one.result bind x f =
  match x with
  | Ok y -> f y
  | Error e -> Error e

let%one.result catch t w =
  match t () with
  | Ok y -> Ok y
  | Error e -> w e

let%one.result fail e =
  Error e

let () =
  let x =
    (
      try%result
        Error 7
      with
        7 ->
        Format.eprintf "Caught 7@.";
        Ok ()
    )
  in
  match x with
  | Ok () -> Format.eprintf "Ok ()@."
  | Error e -> Format.eprintf "Error %d@." e
