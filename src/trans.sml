(* trans.sml *)

structure Trans =
struct

structure A = Absyn
structure S = Symbol

val outstream = ref TextIO.stdOut
val line = ref 0
val Rho = ref [] : (S.symbol * int) list ref

fun say s = TextIO.output(!outstream,s)
fun sayln s = (say s; say "\n")

fun lenstr s = Int.toString (List.length s + 1)

fun p_op oper =
    case oper
     of A.PlusOp      => "n_plus"
      | A.MinusOp     => "n_minus"
      | A.XorOp       => "n_xor"
      | A.LtOp        => "n_lt"
      | A.GtOp        => "n_gt"
      | A.AndOp       => "n_and"
      | A.OrOp        => "n_or"
      | A.AndandOp    => "n_andand"
      | A.OrorOp      => "n_oror"
      | A.EqOp        => "n_eq"
      | A.NeqOp       => "n_neq"
      | A.LeOp        => "n_le"
      | A.GeOp        => "n_ge"
      | A.TimesOp     => "n_times"
      | A.DivideOp    => "n_div"
      | A.RemainderOp => "n_mod"
      | A.SmfOp       => "n_smf"
      | A.RShiftOp    => "n_rshift"
      | A.LShiftOp    => "n_lshift"
      | A.RRotateOp   => "n_rrotate"
      | A.LRotateOp   => "n_lrotate"

fun p_modop oper =
    case oper
     of A.M_PlusOp => "n_aop_plus"
      | A.M_MinusOp => "n_aop_minus"
      | A.M_XorOp => "n_aop_xor"
      | A.M_RRotateOp => "n_rrotate"
      | A.M_LRotateOp => "n_lrotate"

(* when lvalue occurs the top of lhs *)
fun p_lvalue rho (A.Lvalue{name,sub,pos,valarray}) =
    let val (str,index) =
            case List.find (fn (x,_) => S.eq(x,name)) rho
             of SOME (_,(n,index)) => (Int.toString n^"\t//(lvalue: " ^ S.name(name) ^")",index)
              | NONE => ("sigma[#num_of "^S.name(name)^"]\t//(lvalue: "^ S.name(name)^")",1)
    in if index=1
       then [str]
       else let val s = ["n_plus",str] @ p_expr rho sub
            in [lenstr s^"\t//n_bop_start"] @ s @ [" -"^lenstr s^"\t//n_bop_end"]
            end
    end
and p_lvalue_not_top rho (A.Lvalue{name,sub,pos,valarray}) =
    let val (str,index) =
            case List.find (fn (x,_) => S.eq(x,name)) rho
             of SOME (_,(n,index)) => (Int.toString n^"\t//(lvalue: " ^ S.name(name) ^")",index)
              | NONE => ("sigma[#num_of "^S.name(name) ^"]\t//(lvalue: " ^ S.name(name) ^")",1)
    in if index=1
       then [str]
       else let val s = [str] @ p_expr rho sub
            in [lenstr s^"\t//n_arr_start"] @ s @ [" -" ^ lenstr s ^ "\t//n_arr_end"]
            end
    end

and p_expr rho (A.VarExp l) = p_lvalue_not_top rho l
  | p_expr rho (A.IntExp i) = [Int.toString(LargeWord.toIntX i) ^ "\t//n_con"]
  | p_expr rho (A.OpExp{left,oper,right,pos}) =
    let val args = [p_op oper ^ "\t//n_bop"] @ p_expr rho left @ p_expr rho right
    in [lenstr args ^ "\t//n_bop_start"] @ args @ [" -" ^ lenstr args ^ "\t//n_bop_end"]
    end

fun p_stms rho (ss) =
    let fun p_stm' s = let val out = p_stm rho s
                           val _ = line := !line + length out
                       in out
                       end
    in List.concat (List.map p_stm' ss)
    end

and p_stm rho (A.ModStm{lvalue=l,modoper=m,exp=e,pos}) =
    let val s = [p_modop m] @ p_lvalue rho l @ p_expr rho e
    in [lenstr s^"\t//n_aop_start"] @ s @ [" -"^lenstr s^"\t//n_aop_end"]
    end
  | p_stm rho (A.IfStm{test1,then',else',test2,pos}) =
    let val e1 = p_expr rho test1
        val s1 = case then'
                  of NONE => ["0\t//n_skip"]
                   | SOME ss => p_stms rho ss
        val s2 = case else'
                  of NONE => ["0\t//n_skip"]
                   | SOME ss => p_stms rho ss
        val e2 = p_expr rho test2
    in [lenstr e1^"\t//n_if_start"] @ e1 @ [" -"^lenstr e1^"\t//n_if_end"] @
       [lenstr s1^"\t//n_stmt_start"]    @ s1 @ [" -"^lenstr s1^"\t//n_stmt_end"] @
       [lenstr s2^"\t//n_stmt_start"]    @ s2 @ [" -"^lenstr s2^"\t//n_stmt_end"] @
       [lenstr e2^"\t//n_fi_start"] @ e2 @ [" -"^lenstr e2^"\t//n_fi_end"]
    end
  | p_stm rho (A.DoStm{test1,do',loop,test2,pos}) =
    let val e1 = p_expr rho test1
        val s1 = case do'
                  of NONE => ["0\t//n_skip"]
                   | SOME ss => p_stms rho ss
        val s2 = case loop
                  of NONE => ["0\t//n_skip"]
                   | SOME ss => p_stms rho ss
        val e2 = p_expr rho test2
    in [lenstr e1^"\t//n_from_start"]  @ e1 @ [" -"^lenstr e1^"\t//n_from_end"] @
       [lenstr s1^"\t//n_stmt_start"]       @ s1 @ [" -"^lenstr s1^"\t//n_stmt_end"] @
       [lenstr s2^"\t//n_stmt_start"]       @ s2 @ [" -"^lenstr s2^"\t//n_stmt_end"] @
       [lenstr e2^"\t//n_until_start"] @ e2 @ [" -"^lenstr e2^"\t//n_until_end"]
    end
  | p_stm rho (A.CallStm{name,pos,proc}) =
    (case List.find (fn (x,_) => S.eq(x,name)) (!Rho)
      of SOME (_,n) => [Int.toString n ^ "\t//call to " ^ S.name(name)]
       | NONE => ["#call_to "^S.name(name)])
  | p_stm rho (A.UncallStm{name,pos,proc}) =
    (case List.find (fn (x,_) => S.eq(x,name)) (!Rho)
      of SOME (_,n) => [Int.toString n ^ "\t//uncall to " ^ S.name(name)]
       | NONE => ["#uncall_to " ^ S.name(name)])
  | p_stm rho (A.ReadStm{lvalue=l,pos}) = p_lvalue rho l
  | p_stm rho (A.WriteStm{lvalue=l,pos}) = p_lvalue rho l
  | p_stm rho (A.SwapStm{lvalue1,lvalue2,pos}) =
    let val s = p_lvalue rho lvalue1 @ p_lvalue rho lvalue2
    in [lenstr s^"\t//n_swap_start"] @ s @ [" -"^lenstr s^"\t//n_swap_end"]
    end
(*     (p_stm rho (A.ModStm{lvalue=lvalue1,modoper=A.M_XorOp,exp=A.VarExp lvalue2,pos=pos}) @ *)
(*      p_stm rho (A.ModStm{lvalue=lvalue2,modoper=A.M_XorOp,exp=A.VarExp lvalue1,pos=pos}) @ *)
(*      p_stm rho (A.ModStm{lvalue=lvalue1,modoper=A.M_XorOp,exp=A.VarExp lvalue2,pos=pos})) *)
  | p_stm rho(A.ErrorStm{str,pos}) = ["n_error\t//"^str]

and p_proc rho (A.Procedure{name,stms,pos}) =
    let val s = p_stms rho stms
    in [lenstr s^"\t//n_stmt_start " ^ S.name(name)] @
       s @
       [" -"^lenstr s^"\t//n_stmt_end " ^ S.name(name)]
    end

and p_prog rho (A.Program(lvalues,procs)) = List.concat (List.map (p_proc rho) procs)

(* when lvalue occurs the top of lhs *)
fun t_lvalue rho (A.Lvalue{name,sub,pos,valarray}) =
    let val index =
            case List.find (fn (x,_) => S.eq(x,name)) rho
             of SOME (_,(n,index)) => index
              | NONE => 1
    in if index=1
       then ["n_con"]
       else ["n_bop_start\t//(lvalue: "^S.name(name)^")","n_bop","n_con"] @ t_expr rho sub @ ["n_bop_end\t//(lvalue: "^S.name(name)^")"]
    end
and t_lvalue_not_top rho (A.Lvalue{name,sub,pos,valarray}) =
    let val index =
            case List.find (fn (x,_) => S.eq(x,name)) rho
             of SOME (_,(n,index)) => index
              | NONE => 1
    in if index=1
       then ["n_var"]
       else ["n_arr_start\t//(lvalue: "^S.name(name)^")","n_arr"] @ t_expr rho sub @
            ["n_arr_end\t//(lvalue: "^S.name(name)^")"]
    end

and t_expr rho (A.VarExp l) = t_lvalue_not_top rho l
  | t_expr rho (A.IntExp i) = ["n_con\t//" ^ Int.toString(LargeWord.toIntX i)]
  | t_expr rho (A.OpExp{left,oper,right,pos}) =
    ["n_bop_start\t//"^p_op oper] @
    ["n_bop"] @ t_expr rho left @ t_expr rho right @
    ["n_bop_end\t//"^p_op oper]

fun t_stms rho ss = List.concat (List.map (t_stm rho) ss)

and t_stm rho(A.ModStm{lvalue=l,modoper=m,exp=e,pos}) =
    ["n_aop_start","n_aop"] @ t_lvalue rho l @ t_expr rho e @ ["n_aop_end"]
  | t_stm rho(A.IfStm{test1,then',else',test2,pos}) =
    ["n_if_start"] @ t_expr rho test1 @ ["n_if_end", "n_stmt_start"] @
    (case then'
      of NONE => ["n_skip"]
       | SOME ss => t_stms rho ss) @
    ["n_stmt_end", "n_stmt_start"] @
    (case else'
      of NONE => ["n_skip"]
       | SOME ss => t_stms rho ss) @
    ["n_stmt_end", "n_fi_start"] @ t_expr rho test2 @ ["n_fi_end"]
  | t_stm rho(A.DoStm{test1,do',loop,test2,pos}) =
    ["n_from_start"] @ t_expr rho test1 @ ["n_from_end"] @ ["n_stmt_start"] @
    (case do'
      of NONE => ["n_skip"]
       | SOME ss => t_stms rho ss) @
    ["n_stmt_end", "n_stmt_start"] @
    (case loop
      of NONE => ["n_skip"]
       | SOME ss => t_stms rho ss) @
    ["n_stmt_end", "n_until_start"] @ t_expr rho test2 @ ["n_until_end"]
  | t_stm rho(A.CallStm{name,pos,proc}) = ["n_call"]
  | t_stm rho(A.UncallStm{name,pos,proc}) = ["n_uncall"]
  | t_stm rho(A.ReadStm _) = ["n_read"]
  | t_stm rho(A.WriteStm _) = ["n_write"]
  | t_stm rho(A.SwapStm{lvalue1,lvalue2,pos}) =
    let val s = t_lvalue rho lvalue1 @ t_lvalue rho lvalue2
    in ["n_swap_start"] @ s @ ["n_swap_end"]
    end
  | t_stm rho(A.ErrorStm{str,pos}) = ["0"]
(*     (t_stm rho(A.ModStm{lvalue=lvalue1,modoper=A.M_XorOp,exp=A.VarExp lvalue2,pos=pos}) @ *)
(*      t_stm rho(A.ModStm{lvalue=lvalue2,modoper=A.M_XorOp,exp=A.VarExp lvalue1,pos=pos}) @ *)
(*      t_stm rho(A.ModStm{lvalue=lvalue1,modoper=A.M_XorOp,exp=A.VarExp lvalue2,pos=pos})) *)

and t_proc rho (A.Procedure{name,stms,pos}) =
    let val _ = Rho := (name,!line) :: !Rho
        val out = ["n_stmt_start\t//procedure "^S.name(name)] @
                  t_stms rho stms @
                  ["n_stmt_end\t//procedure "^S.name(name)]
        val _ = line := !line+length out
    in out
    end

and t_prog rho (A.Program(lvalues,procs)) = List.concat (List.map (t_proc rho) procs)

fun enum n [] = []
  | enum n (A.Lvalue{name,sub=A.IntExp(w),pos,valarray}::rs) =
    (name,(n,LargeWord.toIntX w)) :: enum (n+LargeWord.toIntX w) rs
  | enum n (A.Lvalue{name,sub=_,pos,valarray}::rs) = ErrorMsg.impossible "in enum"

fun p_print filename (outstream', program0) =
    (outstream := outstream'; line := 0; Rho := [];
     let fun mkrho(A.Program(lvalues,procs)) = enum 0 lvalues
         val rho = mkrho program0
         val type' = t_prog rho program0
         val prog = p_prog rho program0
         val _ = sayln ("type[" ^ Int.toString (length type') ^ "] para[" ^ Int.toString (length prog) ^ "]")
         val _ = say "tmp_stack[5] "
         val _ = sayln ("sigma["^Int.toString (foldl (op +) 0 (map (fn x => #2 (#2 x)) rho))^"]")
         val _ = sayln ""
         val _ = sayln ("procedure "^
                        List.last (String.tokens (fn c => c = #"/")
                                                 (hd (String.tokens (fn c => c = #".")
                                                                    filename))))
         fun t_f n [] = ()
           | t_f n (l::ls) = (sayln ("  type[" ^ Int.toString n ^ "]+=" ^ l);
                              t_f (n+1) ls)
         fun p_f n [] = ()
           | p_f n (l::ls) = (sayln ("  para[" ^ Int.toString n ^ "]+=" ^ l);
                              p_f (n+1) ls)
     in (t_f 0 type';
         sayln "";
         p_f 0 prog)
     end;
     TextIO.flushOut (!outstream))

fun trans filename = p_print filename (TextIO.stdOut,Parse.parse filename)

end
