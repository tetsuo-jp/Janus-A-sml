structure T = TextIO

datatype lexresult
  = VAR of string
  | NUM of int
  | LBRACKET
  | RBRACKET
  | CALL
  | UNCALL
  | SYMBOLS
  | TRACE
  | UNTRACE
  | RESET
  | QUIT
  | EQ
  | NEWLINE
  | TIME
  | ADD
  | STRING of string
  | EOF

val linenum = ref 1
val error = fn x => T.output(T.stdOut,x ^ "\n")
val eof = fn () => EOF
fun inc( i ) = i := !(i) + 1
%%
%structure JanusPrompt
id=[a-zA-Z][a-zA-Z_0-9]*;
str=[\.\/a-zA-Z_0-9]*;
num=[0-9]+;
space=[\t\ ];
%%
\n              => (NEWLINE);
"call"          => (CALL);
"uncall"        => (UNCALL);
"symbols"       => (SYMBOLS);
"trace"         => (TRACE);
"untrace"       => (UNTRACE);
"reset"         => (RESET);
"quit"          => (QUIT);
"time"          => (TIME);
"add"           => (ADD);
{id}            => (VAR yytext);
\"{str}\"       => ((STRING o implode o rev o tl o rev o tl o explode) yytext);
{space}+        => (lex());
"["             => (LBRACKET);
"]"             => (RBRACKET);
{num}           => (NUM (valOf (Int.fromString(yytext))));
"="             => (EQ);
.               => (error ("prompt: ignoring bad character "^yytext); lex());
