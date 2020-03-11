module Sexp = struct
  type t =
    | Atom of string
    | List of t list
end

module Csexp = Csexp.Make (Sexp)

let%expect_test _ =
  Csexp.to_channel stdout (List [ Atom "Hello"; Atom "world!" ]);
  [%expect {| (5:Hello6:world!) |}]

let%expect_test _ =
  let round_trip sexp =
    assert (sexp |> Csexp.to_string |> Csexp.parse_string = Ok sexp)
  in
  round_trip (List [ Atom "Hello"; Atom "world!" ])
