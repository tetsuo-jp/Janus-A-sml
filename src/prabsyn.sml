structure PrintAbsyn :
     sig val print : TextIO.outstream * Absyn.program -> unit
         val proc : Absyn.proc * int -> unit
         val exp : Absyn.exp * int -> unit
         val stm : Absyn.stm * int -> unit
     end =
struct

structure A = Absyn
structure S = Symbol

val outstream = ref TextIO.stdOut

fun say s   =  TextIO.output(!outstream,s)
fun sayln s = (say s; say "\n")

fun indent 0 = ()
  | indent i = (say " "; indent(i-1))

fun opname oper = case oper
 of A.PlusOp      => "PlusOp"
  | A.MinusOp     => "MinusOp"
  | A.XorOp       => "XorOp"
  | A.LtOp        => "LtOp"
  | A.GtOp        => "GtOp"
  | A.AndOp       => "AndOp"
  | A.OrOp        => "OrOp"
  | A.AndandOp    => "AndandOp"
  | A.OrorOp      => "OrorOp"
  | A.EqOp        => "EqOp"
  | A.NeqOp       => "NeqOp"
  | A.LeOp        => "LeOp"
  | A.GeOp        => "GeOp"
  | A.TimesOp     => "TimesOp"
  | A.DivideOp    => "DivideOp"
  | A.RemainderOp => "RemainderOp"
  | A.SmfOp       => "SmfOp"
  | A.RShiftOp    => "RShiftOp"
  | A.LShiftOp    => "LShiftOp"
  | A.RRotateOp   => "RRotateOp"
  | A.LRotateOp   => "LRotate"

fun modoper A.M_PlusOp    = "M_PlusOp"
  | modoper A.M_MinusOp   = "M_MinusOp"
  | modoper A.M_XorOp     = "M_XorOp"
  | modoper A.M_RRotateOp = "M_RRotateOp"
  | modoper A.M_LRotateOp = "M_LRotate"

fun lvalue(A.Lvalue{name,sub,pos,valarray},d) =
    (indent d; sayln "Lvalue(";
     indent (d+1); say (S.name(name)); sayln ",";
     exp(sub,d+1); sayln ",";
     indent (d+1); case !valarray
                    of NONE => say "ref NONE"
                     | SOME _ => say "ref SOME"; say ")")
and exp(A.VarExp l, d) = (indent d; sayln "VarExp("; lvalue(l,d+1); say ")")
  | exp(A.IntExp i, d) = (indent d; say "IntExp("; say(Int.toString(LargeWord.toIntX i)); say ")")
  | exp(A.OpExp{left,oper,right,pos},d) =
    (indent d; sayln "OpExp(";
     indent (d+1); say(opname oper); sayln ",";
     exp(left,d+1); sayln ","; exp(right,d+1); say ")")

fun dolist d f [a] = (sayln ""; f(a,d+1))
  | dolist d f (a::r) = (sayln ""; f(a,d+1); say ","; dolist d f r)
  | dolist d f nil = ()

fun stms(ss,d) = dolist d stm ss
and stm(A.ModStm{lvalue=l,modoper=m,exp=e,pos},d) =
    (indent d; sayln "ModStm(";
     lvalue(l,d+1); sayln ",";
     indent(d+1); say(modoper m); sayln",";
     exp(e,d+1);
     say ")")
  | stm(A.IfStm{test1,then',else',test2,pos},d) =
    (indent d; sayln "IfStm("; exp(test1,d+1); sayln ",";
     case then'
      of NONE => (indent (d+1); say "[]")
       | SOME ss => (indent (d+1); say "["; stms(ss,d+1); say "]");
     sayln ",";
     case else'
      of NONE => (indent (d+1); say "[]")
       | SOME ss => (indent (d+1); say "["; stms(ss,d+1); say "]");
     sayln ","; exp(test2,d+1); say ")")
  | stm(A.DoStm{test1,do',loop,test2,pos},d) =
    (indent d; sayln "DoStm(";
     exp(test1,d+1);
     case do'
      of NONE => (sayln ",";
                  indent (d+1); say "[]")
       | SOME ss => (sayln ",";
                     indent (d+1); say "["; stms(ss,d+1); say "]");
     case loop
      of NONE => (sayln ",";
                  indent (d+1); say "[]")
       | SOME ss => (sayln ",";
                     indent (d+1); say "["; stms(ss,d+1); say "]");
     sayln ",";
     exp(test2,d+1); say ")")
  | stm(A.CallStm{name,pos,proc},d) =
    (indent d; say "CallStm("; say (S.name(name)); say ")")
  | stm(A.UncallStm{name,pos,proc},d) =
    (indent d; say "UncallStm("; say (S.name(name)); say ")")
  | stm(A.ReadStm{lvalue=l,pos},d) =
    (indent d; say "ReadStm("; lvalue(l,d+1); say ")")
  | stm(A.WriteStm{lvalue=l,pos},d) =
    (indent d; say "WriteStm("; lvalue(l,d+1); say ")")
  | stm(A.SwapStm{lvalue1,lvalue2,pos},d) =
    (indent d; sayln "SwapStm(";
     lvalue(lvalue1,d+1); sayln",";
     lvalue(lvalue2,d+1); sayln")")
  | stm(A.ErrorStm{str,pos},d) =
    (indent d; sayln "ErrorStm(";
     indent (d+1); say str; say ")")

and proc(A.Procedure{name,stms,pos},d) =
    (indent d; sayln "Procedure(";
     indent (d+1); sayln (S.name(name) ^ ",");
     indent (d+1); say "["; dolist (d+1) stm stms; say "]";
     say ")")

and program(A.Program(lvalues,procs),d) =
    (indent d; say "Program(";
     dolist d lvalue lvalues;
     dolist d proc procs;
     sayln ")")

fun print (outstream', program0) =
    (outstream := outstream';
     program(program0,0); sayln ""; TextIO.flushOut (!outstream))
end
