type svalue = Tokens.svalue
type pos = int
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) Tokens.token

fun inc(r as ref(n)) = (r := n+1)
fun dec(r as ref(n)) = (r := n-1)

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos

fun err(p1,p2) = ErrorMsg.error p1

fun newLine p = (inc lineNum; linePos := p :: !linePos)

fun eof() = let val pos = hd(!linePos)
            in Tokens.EOF(pos,pos)
            end

fun idToken (t: string, pbegin: pos) =
    let val pend = pbegin + size t
        val be = (pbegin,pend)
     in case t
          of "procedure" => Tokens.PROCEDURE be
           | "if"        => Tokens.IF        be
           | "then"      => Tokens.THEN      be
           | "else"      => Tokens.ELSE      be
           | "fi"        => Tokens.FI        be
           | "from"      => Tokens.FROM      be
           | "do"        => Tokens.DO        be
           | "loop"      => Tokens.LOOP      be
           | "until"     => Tokens.UNTIL     be
           | "call"      => Tokens.CALL      be
           | "uncall"    => Tokens.UNCALL    be
           | "read"      => Tokens.READ      be
           | "write"     => Tokens.WRITE     be
           | "error"     => Tokens.ERROR     be
           | _           => Tokens.ID (t, pbegin, pend)
    end

fun op1Token (t: string, pbegin: pos) =
    let val pend = pbegin + size t
        val be = (pbegin,pend)
     in case t of
          (* Binary Operators *)
            "+"  => (true, Tokens.PLUS be)
          | "-"  => (true, Tokens.MINUS be)
          | "^"  => (true, Tokens.XOR be)
          | "*"  => (true, Tokens.TIMES be)
          | "/"  => (true, Tokens.DIVIDE be)
          | "%"  => (true, Tokens.REMAINDER be)
          | "*/" => (true, Tokens.SMF be)
          | "&"  => (true, Tokens.AND be)
          | "|"  => (true, Tokens.OR be)
          | "&&" => (true, Tokens.ANDAND be)
          | "||" => (true, Tokens.OROR be)
          | "<"  => (true, Tokens.LT be)
          | ">"  => (true, Tokens.GT be)
          | "="  => (true, Tokens.EQ be)
          | "!=" => (true, Tokens.NEQ be)
          | "<=" => (true, Tokens.LE be)
          | ">=" => (true, Tokens.GE be)

          (* Extensions *)
          | ">>" => (true, Tokens.RSHIFT be)
          | "<<" => (true, Tokens.LSHIFT be)
          | ">=>" => (true, Tokens.RROTATE be)
          | "<=<" => (true, Tokens.LROTATE be)

          (* Modification Operators *)
          | "+=" => (true, Tokens.M_PLUS be)
          | "-=" => (true, Tokens.M_MINUS be)
          | "^=" => (true, Tokens.M_XOR be)
          | "<=>" => (true, Tokens.SWAP be)
          | ">=>=" => (true, Tokens.M_RROTATE be)
          | "<=<=" => (true, Tokens.M_LROTATE be)

          (* Unary Operators *)
          | "~"  => (true, Tokens.U_NOT be)

          | _    => (false, Tokens.ID (t, pbegin, pend))
    end

fun parenToken (t: string, pbegin: pos) =
    let val pend = pbegin + size t
        val be = (pbegin,pend)
     in case t of
          (* Punctuations *)
            "("  => Tokens.LPAREN be
          | ")"  => Tokens.RPAREN be
          | "["  => Tokens.LBRACK be
          | "]"  => Tokens.RBRACK be
          | _    => (ErrorMsg.error pbegin "Impossible happen"; Tokens.EOF be)
    end

%%
%header (functor JANUSLexFun(structure Tokens: JANUS_TOKENS));

id=[a-zA-Z][a-zA-Z_0-9]*;
space=[\t\ ];
opchar=("+"|"-"|"^"|"*"|"/"|"%"|"&"|"|"|"<"|">"|"!"|"="|"~");
paren=("("|")"|"["|"]");

%%
"//".*   => (continue());
{id}     => (idToken (yytext,yypos));
{opchar}+ => (let val (b,tok) = op1Token (yytext,yypos)
                in if b
                   then tok
                   else (ErrorMsg.error yypos ("Illegal operator: " ^ yytext); continue())
                end);
[0-9]+   => (Tokens.INT(LargeWord.fromLargeInt(valOf(LargeInt.fromString yytext))
                        handle _ => (ErrorMsg.error yypos "Number too large"; 0w0),
                        yypos,
                        yypos+size yytext));
{space}  => (continue());
{paren}  => (parenToken (yytext,yypos));
\n       => (newLine yypos; continue());
.        => (ErrorMsg.error yypos ("Illegal character: " ^ yytext); continue());
