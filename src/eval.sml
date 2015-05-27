structure Eval =
struct

structure E = Env
structure A = Absyn
structure S = Symbol
structure Err = ErrorMsg
structure T = TextIO
structure W = LargeWord

val trace = ref false

fun bool2word(x): W.word = if x then 0w0-0w1 else 0w0

fun intExp2bool(A.IntExp n) = if n <> 0w0 then true else false
  | intExp2bool _           = Err.impossible "Runtime error0"

fun unIntExp(A.IntExp n) = n
  | unIntExp _           = Err.fatalError 0 "Runtime error1"

fun xor(true,b)  = not b
  | xor(false,b) = b

fun lift1 oper (x,y) = oper(W.toLargeIntX x,W.toLargeIntX y)
fun lift2 oper (x,y) = W.fromLargeInt(oper(W.toLargeIntX x,W.toLargeIntX y))

fun eval_mod(A.M_PlusOp)  = W.+
  | eval_mod(A.M_MinusOp) = W.-
  | eval_mod(A.M_XorOp)   = W.xorb
  | eval_mod(A.M_RRotateOp | A.M_LRotateOp) = Err.unimplemented "in eval_mod"

fun uneval_mod(A.M_PlusOp)  = W.-
  | uneval_mod(A.M_MinusOp) = W.+
  | uneval_mod(A.M_XorOp)   = W.xorb
  | uneval_mod(A.M_RRotateOp | A.M_LRotateOp) = Err.unimplemented "in eval_mod"

fun look_lvalue(env, A.Lvalue{name,sub=exp,pos,valarray}: A.lvalue): S.symbol * W.word =
    case eval(env,exp)
     of A.IntExp(i) => (name,i)
      | _ => Err.fatalError 0 "Runtime error2"
and call(env,name)(proc) = case !proc of SOME stms => exec(env,stms)
                                       | NONE => Err.impossible "in call"
and uncall(env,name)(proc) = case !proc of SOME stms => unexec(env,stms)
                                         | NONE => Err.impossible "in uncall"
and exec_if(env,test1,then',else',test2,pos) =
    let val b = intExp2bool(eval(env,test1))
        val env' = if b then exec_option(env,then')
                        else exec_option(env,else')
        val b' = intExp2bool(eval(env',test2))
    in if b = b' then env'
                 else Err.fatalError pos (
                      "Runtime error4: test (after if) is " ^ Bool.toString b ^
                      " and assertion (after fi) is " ^ Bool.toString b' ^ " in fwd computation.\n" ^
                      "test:      " ^ Pretty.exp(test1) ^ "\n" ^
                      "assertion: " ^ Pretty.exp(test2))
    end
and unexec_if(env,test1,then',else',test2,pos) =
    let val b = intExp2bool(eval(env,test2))
        val env' = if b then unexec_option(env,then')
                        else unexec_option(env,else')
        val b' = intExp2bool(eval(env',test1))
    in if b = b' then env'
                 else Err.fatalError pos (
                      "Runtime error5: test (after fi) is " ^ Bool.toString b ^
                      " and assertion (after if) is " ^ Bool.toString b' ^ " in bwd computation.\n" ^
                      "test:      " ^ Pretty.exp(test2) ^ "\n" ^
                      "assertion: " ^ Pretty.exp(test1))
    end
and exec_do(env,first,test1,do',loop,test2,pos,stm) =
    let val _ = if !trace andalso not first
                then (print "====\n";
                      print(Pretty.s_stm(stm)); print "\n")
                else ()
    in
    if xor(intExp2bool(eval(env,test1)),not first)
    then let val env' = exec_option(env,do')
         in if intExp2bool(eval(env',test2))
            then env'
            else let val env'' = exec_option(env',loop)
                 in exec_do(env'',false,test1,do',loop,test2,pos,stm)
                 end
         end
    else Err.fatalError pos
           ("The assertion (after from) should be " ^ Bool.toString first ^ " in fwd computation.\n" ^
            "assertion: " ^ Pretty.exp(test1))
    end
and unexec_do(env,first,test1,do',loop,test2,pos,stm) =
    let val _ = if !trace andalso not first
                then (print "====\n";
                      print(Pretty.s_stm(stm)); print "\n")
                else ()
    in if xor(intExp2bool(eval(env,test2)),not first)
       then let val env' = unexec_option(env,do')
            in if intExp2bool(eval(env',test1))
               then env'
               else let val env'' = unexec_option(env',loop)
                    in (* gexec1(backward())(env'',stm) *)
                        unexec_do(env'',false,test1,do',loop,test2,pos,stm)
                    end
            end
       else Err.fatalError pos
              ("The assertion (after until) should be " ^ Bool.toString first ^ " in bwd computation.\n" ^
               "assertion: " ^ Pretty.exp(test2))
    end
and exec_option(env,SOME stms) = exec(env,stms)
  | exec_option(env,NONE     ) = env
and unexec_option(env,SOME stms) = unexec(env,stms)
  | unexec_option(env,NONE     ) = env
and exec(env: E.env,stm::stms): E.env = exec(gexec1(forward())(env,stm),stms)
  | exec(env,[]) = env
and unexec(env,stm::stms) = let val env' = unexec(env,stms)
                            in gexec1(backward())(env',stm)
                            end
  | unexec(env,[]) = env
and forward()  = {exec_if=exec_if,  exec_do=exec_do,  call=call,  uncall=uncall, eval_mod=eval_mod}
and backward() = {exec_if=unexec_if,exec_do=unexec_do,call=uncall,uncall=call,   eval_mod=uneval_mod}
and gexec1 dir (env: E.env,stm): E.env =
    let val _ = if !trace
                then (print "====\n";
                      print(Pretty.s_stm(stm)); print "\n")
                else ()
        val env2 = case stm
     of A.ModStm{lvalue,modoper,exp,pos} =>
        (let val (name,i): S.symbol * W.word = look_lvalue(env,lvalue)
             val lv: W.word = unIntExp(eval_lvalue(env,lvalue))
             val rv: W.word = unIntExp(eval(env,exp))
             val env': E.env = E.update_var(env,name,i,#eval_mod(dir)(modoper)(lv,rv))
                 handle Overflow => Err.impossible
                                        ("eval 2: "^W.toString lv^Pretty.modoper modoper^W.toString rv)
             val _ = (if !trace
                      then (print "\n"; Env.print_allvars(env'))
                      else ())
         in env'
         end
         handle Overflow => Err.fatalError 0 "overflow in eval")
      | A.IfStm{test1,then',else',test2,pos} =>
        #exec_if(dir)(env,test1,then',else',test2,pos)
      | A.DoStm{test1,do',loop,test2,pos} =>
        #exec_do(dir)(env,true,test1,do',loop,test2,pos,stm)
      | A.CallStm{name,pos,proc} => #call(dir)(env,name)(proc)
      | A.UncallStm{name,pos,proc} => #uncall(dir)(env,name)(proc)
      | A.ReadStm{lvalue,pos} =>
        let val (name,i) = look_lvalue(env,lvalue)
            fun read() = W.fromInt(valOf(Int.fromString(valOf(T.inputLine(T.stdIn)))))
                handle Error => (print "Invalid input. Type again.\n"; read())
            val env' = E.update_var(env,name,i,read())
            val _ = (if !trace then (print "\n"; Env.print_allvars(env')) else ())
        in env'
        end
      | A.WriteStm{lvalue,pos} =>
        let val (name,i) =look_lvalue(env,lvalue)
            val v = E.look_var(env,name,i)
            val num = W.toLargeIntX v
            val _ = print((if num < 0 then "-" else "") ^ LargeInt.toString(abs(num)) ^ "\n")
        in env
        end
      | A.SwapStm{lvalue1,lvalue2,pos} =>
        let val (n1,i1) = look_lvalue(env,lvalue1)
            val (n2,i2) = look_lvalue(env,lvalue2)
            val v1 = E.look_var(env,n1,i1)
            val v2 = E.look_var(env,n2,i2)
            val env'  = E.update_var(env,n1,i1,v2)
            val env'' = E.update_var(env',n2,i2,v1)
            val _ = if !trace then (print "\n"; Env.print_allvars(env')) else ()
        in env''
        end
      | A.ErrorStm{str,pos} => (Err.fatalError pos ("error: " ^ str))
    in env2
    end
    handle Overflow => Err.impossible "eval 92"
and eval_lvalue(env,A.Lvalue{name,sub,pos,valarray}: A.lvalue) =
    case eval(env,sub)
     of A.IntExp(i) =>
        A.IntExp(case !valarray
                  of SOME ls => Array.sub(ls,W.toIntX i)
                   | NONE    => Err.impossible ("in Eval.eval_lvalue: Name="^S.name name)
                )
      | e => Err.fatalError 0 ("Runtime error3 in eval_lvalue: \n"
             before (PrintAbsyn.exp (e,0); print "\n"))
and eval(env,exp) = case exp
 of A.VarExp lvalue => eval_lvalue(env,lvalue)
  | A.IntExp n => A.IntExp n
  | A.OpExp{left,oper,right,pos} =>
    let val lv = unIntExp(eval(env,left))
        val rv = unIntExp(eval(env,right))
        val op' = case
            oper
         of A.PlusOp => W.+
          | A.MinusOp => W.-
          | A.XorOp => W.xorb
          | A.LtOp => bool2word o lift1 LargeInt.<
          | A.GtOp => bool2word o lift1 LargeInt.>
          | A.AndOp => W.andb
          | A.OrOp => W.orb
          | A.AndandOp => (fn (a,b) => bool2word (a<>0w0 andalso b<>0w0))
          | A.OrorOp => (fn (a,b) => bool2word (a<>0w0 orelse b<>0w0))
          | A.EqOp => bool2word o op =
          | A.NeqOp => bool2word o not o op =
          | A.LeOp => bool2word o lift1 LargeInt.<=
          | A.GeOp => bool2word o lift1 LargeInt.>=
          | A.TimesOp => lift2 LargeInt.*
          | A.DivideOp => lift2 LargeInt.div
          | A.RemainderOp => lift2 LargeInt.mod
          | A.SmfOp => (* smf *)
            (fn (m1,m2) =>
                let val m1p = if lift1 LargeInt.< (m1,0w0) then 0w0-m1 else m1
                    val m2p = if lift1 LargeInt.< (m2,0w0) then 0w0-m2 else m2
                    val maskInit = W.<<(0w1: W.word,Word.fromInt (W.wordSize - 1))
                    val positionInit = 0w1
                    fun smf 0w32     mask = 0w0
                      | smf position mask
                        = let val mask' = W.>>(mask,0w1)
                          in if W.andb(m1p,mask') <> 0w0
                             then W.>>(m2p,position) + smf (position+0w1) mask'
                             else smf (position+0w1) mask'
                          end
                    val p = smf positionInit maskInit
                    val p' = if lift1 LargeInt.< (m1,0w0) then 0w0-p else p
                    val p''= if lift1 LargeInt.< (m2,0w0) then 0w0-p else p'
                in p''
                end
            handle Overflow => Err.impossible "eval 1")
          | A.RShiftOp => (fn (w1,w2) => W.>> (w1,Word.fromLargeWord w2))
          | A.LShiftOp => (fn (w1,w2) => W.<< (w1,Word.fromLargeWord w2))
          | A.RRotateOp => Err.unimplemented "in eval"
          | A.LRotateOp => Err.unimplemented "in eval"
    in A.IntExp (op' (lv,rv))
       handle Domain => (print ("ERROR: The input real is NaN. 0wx" ^
                                W.toString lv ^ " " ^ Pretty.opname oper ^ " 0wx" ^ W.toString rv ^ "\n");
                         raise Err.Dump)
            | Overflow => (print ("ERROR: Overflow. 0wx" ^
                                  W.toString lv ^ " " ^ Pretty.opname oper ^ " 0wx" ^ W.toString rv ^ "\n");
                           raise Err.Dump)
    end

end
