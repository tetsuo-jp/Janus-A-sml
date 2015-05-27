structure Pretty =
struct

structure A = Absyn
structure S = Symbol
val outstream = ref TextIO.stdOut

fun indent 0 = ""
  | indent i = " " ^ indent(i-1)

fun opname oper = case oper
 of A.PlusOp      => "+"
  | A.MinusOp     => "-"
  | A.XorOp       => "^"
  | A.LtOp        => "<"
  | A.GtOp        => ">"
  | A.AndOp       => "&"
  | A.OrOp        => "|"
  | A.AndandOp    => "&&"
  | A.OrorOp      => "||"
  | A.EqOp        => "="
  | A.NeqOp       => "!="
  | A.LeOp        => "<="
  | A.GeOp        => ">="
  | A.TimesOp     => "*"
  | A.DivideOp    => "/"
  | A.RemainderOp => "%"
  | A.SmfOp       => "*/"
  | A.RShiftOp    => ">>"
  | A.LShiftOp    => "<<"
  | A.RRotateOp   => ">=>"
  | A.LRotateOp   => "<=<"

fun modoper A.M_PlusOp = "+="
  | modoper A.M_MinusOp = "-="
  | modoper A.M_XorOp = "^="
  | modoper A.M_RRotateOp = ">=>="
  | modoper A.M_LRotateOp = "<=<="

fun lvalue(A.Lvalue{name,sub,pos,valarray}) =
    if exp sub = "0" then Symbol.name(name)
    else Symbol.name(name) ^ "[" ^ exp sub ^ "]"
and exp(A.VarExp l) = lvalue(l)
  | exp(A.IntExp i) = Int.toString (LargeWord.toInt i) (* "0x" ^ LargeWord.toString i *)
  | exp(A.OpExp{left,oper,right,pos}) =
    "(" ^ exp left ^ " " ^ opname oper ^ " " ^ exp right ^ ")"

fun ifsome (SOME _) s = s
  | ifsome _        s = ""

(* simple version *)
fun s_stm(A.IfStm{test1,then',else',test2,pos}) =
    "if " ^ exp(test1) ^ "\n" ^
    ifsome then' "    then {stm}*" ^ "\n" ^
    ifsome else' "    else {stm}*" ^ "\n" ^
    "fi " ^ exp(test2)
  | s_stm(A.DoStm{test1,do',loop,test2,pos}) =
    "from " ^ exp(test1) ^ "\n" ^
    ifsome do'  "    do {stm}*" ^ "\n" ^
    ifsome loop "    loop {stm}*" ^ "\n" ^
    "until " ^ exp(test2)
  | s_stm(A.CallStm{name,pos,proc}) = "call " ^ S.name(name)
  | s_stm(A.UncallStm{name,pos,proc}) = "uncall " ^ S.name(name)
  | s_stm(A.ReadStm{lvalue=l,pos}) = "read " ^ lvalue(l)
  | s_stm(A.WriteStm{lvalue=l,pos}) = "write " ^ lvalue(l)
  | s_stm(A.ModStm{lvalue=l,modoper=m,exp=e,pos}) =
    lvalue(l) ^ " " ^ modoper m ^ " " ^ exp(e)
  | s_stm(A.SwapStm{lvalue1,lvalue2,pos}) =
    lvalue(lvalue1) ^ " <=> " ^ lvalue(lvalue2)
  | s_stm(A.ErrorStm{str,pos}) = "error " ^ str

fun interleave (s:string) ([]:string list) = ""
  | interleave (s:string) ([x]:string list) = x
  | interleave s (s'::ss) = s' ^ s ^ interleave s ss

fun ifsome' (SOME ss) f = f ss
  | ifsome' _         f = ""

(* verbatim version *)
fun v_stm(A.IfStm{test1,then',else',test2,pos}) =
    "if " ^ exp(test1) ^ "\n  " ^
    (ifsome' then' (fn ss => "then\n  " ^ interleave "\n  " (map v_stm ss)) ^ "\n  ") ^
    (ifsome' else' (fn ss => "else\n  " ^ interleave "\n  " (map v_stm ss)) ^ "\n  ") ^
    "fi " ^ exp(test2) ^ "\n"
  | v_stm(A.DoStm{test1,do',loop,test2,pos}) =
    "from " ^ exp(test1) ^  "\n  " ^
    (ifsome' do'   (fn ss => "do\n" ^ interleave "\n  " (map v_stm ss)) ^ "\n  ") ^
    (ifsome' loop  (fn ss => "loop\n  " ^ interleave "\n  " (map v_stm ss)) ^ "\n  ") ^
    "until " ^ exp(test2)
  | v_stm(s) = s_stm(s)

fun proc (A.Procedure{name,stms,pos}) =
    "procedure " ^ S.name(name) ^ "\n  " ^
    interleave "\n  " (map v_stm stms)

fun program (A.Program(ls,ps)) =
    interleave " " (map lvalue ls) ^ "\n\n" ^
    interleave "\n\n" (map proc ps) ^ "\n"

fun p_print filename (outstream', program0) =
    (outstream := outstream';
     TextIO.output(!outstream,program program0);
     TextIO.flushOut (!outstream))

fun test filename = p_print filename (TextIO.stdOut,Parse.parse filename)
end
