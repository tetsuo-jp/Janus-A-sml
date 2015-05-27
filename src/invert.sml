(* A program inverter *)

structure Invert =
struct

structure A = Absyn
val outstream = ref TextIO.stdOut

fun moper A.M_PlusOp = A.M_MinusOp
  | moper A.M_MinusOp = A.M_PlusOp
  | moper A.M_XorOp = A.M_XorOp
  | moper A.M_RRotateOp = A.M_LRotateOp
  | moper A.M_LRotateOp = A.M_RRotateOp

fun stm s = case s
 of A.ModStm{lvalue,modoper,exp,pos} =>
    A.ModStm{lvalue=lvalue,modoper=moper modoper,exp=exp,pos=pos}
  | A.IfStm {test1,then',else',test2,pos} =>
    A.IfStm {test1=test2,
             then'=Option.map (rev o map stm) then',
             else'=Option.map (rev o map stm) else',
             test2=test1,
             pos=pos}
  | A.DoStm {test1, do', loop, test2, pos} =>
    A.DoStm {test1=test2,
             do' =Option.map (rev o map stm) do',
             loop=Option.map (rev o map stm) loop,
             test2=test1,
             pos=pos}
  | (A.CallStm _ | A.UncallStm _ | A.ReadStm _ | A.WriteStm _ | A.SwapStm _ | A.ErrorStm _) => s
fun proc (A.Procedure{name,stms,pos}) = A.Procedure{name=name,stms=rev (map stm stms),pos=pos}
fun program (A.Program(ls,ps)) = (A.Program(ls,map proc ps))

fun p_print filename (outstream', program0) =
    (outstream := outstream';
     TextIO.output(!outstream,Pretty.program (program program0));
     TextIO.flushOut(!outstream))

fun invert filename = p_print filename (TextIO.stdOut,Parse.parse filename)

end
