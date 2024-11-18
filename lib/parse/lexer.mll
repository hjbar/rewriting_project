{

  open Lexing
  open Parser

}

let digit  = ['0'-'9']
let number = ['-']? digit+
let alpha  = ['a'-'z' 'A'-'Z']
let ident  = alpha (alpha | '_' | digit)*
let word   = ['a'-'z']+

rule token = parse
  | ['\n']            { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+  { token lexbuf                  }

  | "//" [^ '\n']* "\n"  { new_line lexbuf; token lexbuf }
  | "/*"                 { comment lexbuf ; token lexbuf }

  | word  as w   { WORD  w  }
  | ident as id  { IDENT id }

  | "-->"  { TO   }
  | ":"    { COLO }

  | ";;"  { SEMI }
  | "**"  { STAR }
  | "##"  { HTAG }

  | eof  { EOF                                               }
  | _    { failwith ("unknown character : " ^ lexeme lexbuf) }


and comment = parse
  | "*/"  { ()                              }
  | _     { comment lexbuf                  }
  | eof   { failwith "unterminated comment" }
