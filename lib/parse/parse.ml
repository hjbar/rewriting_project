let open_in_file file = open_in_gen [ Open_rdonly; Open_creat ] 0o666 file

let create_dir dir =
  if not @@ Sys.file_exists dir then Sys.command @@ Format.sprintf "mkdir -p %s" dir |> ignore

let parse_file dir filename =
  create_dir dir;
  let out_c = open_in_file @@ Format.sprintf "%s/%s" dir filename in

  try
    let lb = Lexing.from_channel out_c in
    let prog = Parser.program Lexer.token lb in
    close_in out_c;
    prog
  with exn ->
    begin
      close_in out_c;
      Format.printf "Error during parsing %s :\n%!" filename;
      raise exn
    end
