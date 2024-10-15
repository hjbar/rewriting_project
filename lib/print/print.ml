(* Codes *)

let red = "\027[31m"

let green = "\027[32m"

let blue = "\027[36m"

let purple = "\027[35m"

let reset = "\027[0m"

(* Some functions on strings *)

let red_string s = Format.sprintf "%s%s%s" red s reset

let green_string s = Format.sprintf "%s%s%s" green s reset

let blue_string s = Format.sprintf "%s%s%s" blue s reset

let purple_string s = Format.sprintf "%s%s%s" purple s reset

(* Some functions of printing *)

let print_newline () = Format.printf "\n%!"

let println_newline () = Format.printf "\n\n%!"

let print_flush = Format.printf "%s%!"

let println_flush = Format.printf "%s\n%!"

let print_error s = red_string s |> print_flush

let println_error s = red_string s |> println_flush

let print_ok s = green_string s |> print_flush

let println_ok s = green_string s |> println_flush

let print_data s = blue_string s |> print_flush

let println_data s = blue_string s |> println_flush

let print_warning s = purple_string s |> print_flush

let println_warning s = purple_string s |> println_flush
