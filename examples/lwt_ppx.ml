let%one.lwt bind = Lwt.bind
let%one.lwt catch = Lwt.catch
let%one.lwt fail = Lwt.fail

let six =
  let five = Lwt.return 5 in
  let%lwt five = five in
  Lwt.return (five + 1)

let () =
  Lwt_main.run six
  |> Format.printf "Six = %d@."
