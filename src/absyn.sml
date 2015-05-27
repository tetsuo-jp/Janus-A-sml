(* absyn.sml *)

structure Absyn =
struct

structure W = LargeWord

(* Values *)
type valarray = W.word Array.array

(* Abstract Syntax Tree *)
type pos = int
datatype oper
  = PlusOp | MinusOp | XorOp | LtOp | GtOp
  | AndOp | OrOp | AndandOp | OrorOp | EqOp | NeqOp | LeOp | GeOp | TimesOp
  | DivideOp | RemainderOp
  | SmfOp | RShiftOp | LShiftOp | RRotateOp | LRotateOp
datatype modoper = M_PlusOp | M_MinusOp | M_XorOp | M_RRotateOp | M_LRotateOp

(* To speed up, a name of lvalue can be pointed by reference
                                 instead of using a table lookup *)
datatype lvalue = Lvalue of {name: Symbol.symbol, sub: exp, pos: pos, valarray: valarray option ref}
and exp
  = VarExp of lvalue
  | IntExp of W.word
  | OpExp of {left: exp, oper: oper, right: exp, pos: pos}
datatype stm
  = ModStm of {lvalue: lvalue, modoper: modoper, exp: exp, pos: pos}
  | IfStm of {test1: exp, then': stm list option, else': stm list option, test2: exp, pos: pos}
  | DoStm of {test1: exp, do': stm list option, loop: stm list option, test2: exp, pos: pos}

  (* To speed up, a body of a procedure is pointed by `proc'
                              instead of using a table lookup *)
  | CallStm of {name: Symbol.symbol, pos: pos, proc: stm list option ref}
  | UncallStm of {name: Symbol.symbol, pos: pos, proc: stm list option ref}

  | ReadStm of {lvalue: lvalue, pos: pos}
  | WriteStm of {lvalue: lvalue, pos: pos}
  | SwapStm of {lvalue1: lvalue, lvalue2: lvalue, pos: pos}
  | ErrorStm of {str: string, pos: pos}
datatype proc = Procedure of {name: Symbol.symbol, stms: stm list, pos: pos}
datatype program = Program of lvalue list * proc list

end (* structure Absyn *)
