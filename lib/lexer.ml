type token =
  | INT of int
  | IDENT of string
  | FN
  | LET
  | IF
  | ELSE
  | WHILE
  | RETURN
  | PRINT
  | TRUE
  | FALSE
  | BREAK
  | CONTINUE
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | SEMI
  | ASSIGN
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | PERCENT
  | EQEQ
  | NEQ
  | LT
  | LE
  | GT
  | GE
  | ANDAND
  | OROR
  | BANG
  | EOF

exception Error of string * int

let keywords =
  [
    ("fn", FN);
    ("let", LET);
    ("if", IF);
    ("else", ELSE);
    ("while", WHILE);
    ("return", RETURN);
    ("print", PRINT);
    ("true", TRUE);
    ("false", FALSE);
    ("break", BREAK);
    ("continue", CONTINUE);
  ]

let is_digit c = c >= '0' && c <= '9'
let is_ident_start c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_ident_char c = is_ident_start c || is_digit c

let two_char = function
  | "==" -> Some EQEQ
  | "!=" -> Some NEQ
  | "<=" -> Some LE
  | ">=" -> Some GE
  | "&&" -> Some ANDAND
  | "||" -> Some OROR
  | _ -> None

let tokenize src =
  let n = String.length src in
  let toks = ref [] in
  let line = ref 1 in
  let push t = toks := (t, !line) :: !toks in
  let rec scan_while p i = if i < n && p src.[i] then scan_while p (i + 1) else i in
  let rec skip_comment i = if i >= n || src.[i] = '\n' then i else skip_comment (i + 1) in
  let rec go i =
    if i >= n then push EOF
    else
      match src.[i] with
      | '\n' ->
          incr line;
          go (i + 1)
      | ' ' | '\t' | '\r' -> go (i + 1)
      | '/' when i + 1 < n && src.[i + 1] = '/' -> go (skip_comment i)
      | c when is_digit c ->
          let j = scan_while is_digit i in
          let text = String.sub src i (j - i) in
          (match int_of_string_opt text with
          | Some v -> push (INT v)
          | None -> raise (Error ("integer literal out of range: " ^ text, !line)));
          go j
      | c when is_ident_start c ->
          let j = scan_while is_ident_char i in
          let text = String.sub src i (j - i) in
          push (match List.assoc_opt text keywords with Some k -> k | None -> IDENT text);
          go j
      | c -> (
          let pair = if i + 1 < n then two_char (String.sub src i 2) else None in
          match pair with
          | Some t ->
              push t;
              go (i + 2)
          | None ->
              let t =
                match c with
                | '(' -> LPAREN
                | ')' -> RPAREN
                | '{' -> LBRACE
                | '}' -> RBRACE
                | ',' -> COMMA
                | ';' -> SEMI
                | '=' -> ASSIGN
                | '+' -> PLUS
                | '-' -> MINUS
                | '*' -> STAR
                | '/' -> SLASH
                | '%' -> PERCENT
                | '<' -> LT
                | '>' -> GT
                | '!' -> BANG
                | _ -> raise (Error (Printf.sprintf "unexpected character '%c'" c, !line))
              in
              push t;
              go (i + 1))
  in
  go 0;
  List.rev !toks
