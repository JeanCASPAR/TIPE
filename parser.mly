%token <string> ID
%token DOT
%token HASHTAG
%token NEWLINE
%token QED
%token COLON
%token THEOREM_TYPE
%token SEQUENT_TYPE
%token ZERO
%token <string>VAR
%token SUCC
%token ADD
%token MUL
%token ATOM
%token NEG
%token AND
%token OR
%token IMPLIES
%token FORALL
%token EXISTS
%token EQUAL
%token LPAR
%token RPAR
%token COMMA
%token TURNSTIL
%%

%start <Sequent.proof list> file
%%

file: f = rev_file { List.rev f };

rev_file:
  | { [] }
  | EOF { [] }
  | f = rev_file; QED; DOT; s = section {s :: f};

section:
  HASHTAG ; HASHTAG ; HASHTAG ; name = ID ; NEWLINE ;
  ty = section_type ;
  l = instruction_list ;
  QED ; DOT {
    Section {
      name;
      ty;
      instr=l;
    }
  };

ty:
  | THEOREM_TYPE ; COLON ; f = formula ; DOT { TheoremType f }
  | SEQUENT_TYPE ; COLON ; s = sequent ; DOT { SequentType s };

instruction_list: l = rev_instr_l { List.rev l };
rev_instr_l:
  | { [] }
  | l = rev_instr_l ; i = instr ; DOT { i :: l};

instr:
  p = peano { Peano p }
  | l = logical { Logical l }
  | e = equality { Equality e };

peano:
  PO { PO }
  | P1 { P1 }
  | P2 { P2 }
  | P3 { P3 }
  | P4 { P4 }
  | P5 { P5 }
  | P6 { P6 }
  | P7 { P7 }
  | REC ; f = formula { Rec f };

term:
  x = VAR { Var x }
  | ZERO { Zero }
  | LPAR ; SUCC ; x  = term ; RPAR { Succ x }
  | LPAR ; x = term ; ADD ; y = term ; RPAR { Add (x, y) }
  | LPAR ; x = term ; MUL ; y = term ; RPAR { Mul (x, y) };

formula:
  p = ATOM { Atom p }
  | LPAR ; NEG ; p = formula ; RPAR { Neg p }
  | LPAR ; p = formula ; AND ; q = formula ; RPAR { And (p, q) }
  | LPAR ; p = formula ; OR ; q = formula ; RPAR { Or (p, q) }
  | LPAR ; p = formula ; IMPLIES ; q = formula ; RPAR { Implies (p, q) }
  | LPAR ; FORALL ; x = VAR ; DOT ; LPAR ; p = formula ; RPAR ; RPAR { Forall (x, p) }
  | LPAR ; EXISTS ; x = VAR ; DOT ; LPAR ; p = formula ; RPAR ; RPAR { Exists (x, p) }
  | LPAR ; x = term ; EQUAL ; y = term ; RPAR { Equal (x, y) };

formula_list: l = rev_formula_list { List.rev l };
rev_formula_list:
  | { [] }
  | l = rev_formula_list ; f = formula ; COMMA { f :: l };

sequent:
  top = formula_list ; TURNSTIL ; bottom = formula { Sequent {
    top;
    bottom
  }};