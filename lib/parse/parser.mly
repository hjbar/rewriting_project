(* LISTE LEXEMES *)
%token <string> IDENT
%token <string> WORD
%token COLO TO
%token SEMI STAR HTAG
%token EOF


(* POINT D'ENTREE *)
%start program
%type <Expr.program> program

%%


let program :=
  | EOF;                  { Hashtbl.create 16 }
  | rslist=hashtbl; EOF;  { rslist }

let hashtbl :=
  | rs=rs; STAR; rs_completed=rs; HTAG; {

    let htbl = Hashtbl.create 16 in
    Hashtbl.replace htbl rs rs_completed;
    htbl

  }
  | rs=rs; STAR; rs_completed=rs; HTAG; h=hashtbl; {

    Hashtbl.replace h rs rs_completed;
    h

  }

let rs :=
  | rs=nonempty_list(rule); { rs }

let rule :=
  | name=IDENT; COLO; w1=WORD; TO; w2=WORD; SEMI; { (Some name, w1, w2) }
