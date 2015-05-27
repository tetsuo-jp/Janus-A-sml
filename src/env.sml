(* env.sml *)

signature ENV =
sig
    type varenv
    type progenv
    type env
    val set_varenv : env * varenv -> env
    val append_varenv : varenv * varenv -> varenv
    val set_progenv : env * progenv -> env
    val look_proc : env * Symbol.symbol -> Absyn.stm list
    val look_var : env * Symbol.symbol * LargeWord.word -> LargeWord.word
    val update_var : env * Symbol.symbol * LargeWord.word * LargeWord.word -> env
    val reset_varenv : env * Symbol.symbol -> env
    val print_vars : env * Symbol.symbol -> unit
    val print_allvars : env -> unit
    val set_initial_env: env -> unit
    val get_initial_env: unit -> env
    val program2env: Absyn.program -> env
    val typecheck: env -> unit
end

structure Env: ENV =
struct

structure Err = ErrorMsg
structure A = Absyn
structure S = Symbol
structure W = LargeWord
structure Map = BinaryMapFn(struct type ord_key = S.symbol
                                   val compare = S.compare end)

(* Environment *)
type varenv  = A.valarray Map.map
type progenv = A.proc list
type env = {varenv: varenv,progenv: progenv}

fun append_varenv (varenv1,varenv2) =
    foldr Map.insert' Map.empty (Map.listItemsi varenv1 @ Map.listItemsi varenv2)
fun set_varenv  ({varenv,progenv},ls) = {varenv=ls,    progenv=progenv}
fun set_progenv ({varenv,progenv},ps) = {varenv=varenv,progenv=ps     }

fun look_proc' progenv name' =
    case List.find (fn (A.Procedure{name,stms,pos}) => S.eq(name,name')) progenv
     of SOME (A.Procedure{name,stms,pos}) => SOME stms
      | NONE => NONE
fun look_proc ({varenv,progenv}: env,name': S.symbol) =
    case look_proc' progenv name'
     of SOME stms => stms
      | NONE => Err.noProc 0 (S.name(name') ^ " is not found. in look_proc")

fun look_arr ({varenv,progenv}: env,name': S.symbol): A.valarray =
    case Map.find (varenv,name')
     of SOME ls => ls
      | NONE => Err.fatalError 0 ("No such name: " ^ S.name(name') ^ " in look_arr")
fun look_var (env as {varenv,progenv}: env,name': S.symbol,i: W.word): W.word =
    let val ls = look_arr (env,name')
    in (Array.sub(ls,W.toIntX(i))
        handle Subscript => Err.fatalError 0
                            ("Subscript 1:"^S.name(name')^"["^Int.toString(W.toIntX(i))^"]"))
    end

fun tie_lvalue (env as {varenv,progenv}) (A.Lvalue {name,sub,pos,valarray}) =
    let val valarray' = case !valarray
                         of NONE => ref (SOME (look_arr (env,name)))
                          | SOME x => valarray
    in A.Lvalue {name=name,sub=tie_exp env sub,pos=pos,valarray=valarray'}
    end
and tie_exp env exp =
    case exp
     of A.VarExp lvalue => A.VarExp (tie_lvalue env lvalue)
      | A.IntExp _ => exp
      | A.OpExp {left,oper,right,pos} =>
        A.OpExp {left=tie_exp env left,oper=oper,right=tie_exp env right,pos=pos}
fun tie_stms (env as {varenv,progenv}) stm =
    case stm
     of A.ModStm {lvalue,modoper,exp,pos} =>
        A.ModStm {lvalue=tie_lvalue env lvalue,modoper=modoper,exp=tie_exp env exp,pos=pos}
      | A.IfStm{test1,then',else',test2,pos} =>
        A.IfStm{test1=tie_exp env test1,
              then'=Option.map (map (tie_stms env)) then',
              else'=Option.map (map (tie_stms env)) else',
              test2=tie_exp env test2,pos=pos}
      | A.DoStm{test1,do',loop,test2,pos} =>
        A.DoStm{test1=tie_exp env test1,
                do' =Option.map (map (tie_stms env)) do',
                loop=Option.map (map (tie_stms env)) loop,
                test2=tie_exp env test2,pos=pos}
      | A.CallStm{name,pos,proc} =>
        let val _ = proc := look_proc' progenv name
        in A.CallStm{name=name,pos=pos,proc=proc}
        end
      | A.UncallStm{name,pos,proc} =>
        let val _ = proc := look_proc' progenv name
        in A.UncallStm{name=name,pos=pos,proc=proc}
        end
      | A.ReadStm {lvalue,pos} => A.ReadStm {lvalue=tie_lvalue env lvalue,pos=pos}
      | A.WriteStm {lvalue,pos} => A.WriteStm {lvalue=tie_lvalue env lvalue,pos=pos}
      | A.ErrorStm _ => stm
      | A.SwapStm {lvalue1,lvalue2,pos} =>
        A.SwapStm {lvalue1=tie_lvalue env lvalue1,lvalue2=tie_lvalue env lvalue2,pos=pos}
fun tie_progenv' env (A.Procedure{name,stms,pos}) =
    A.Procedure{name=name,stms=map (tie_stms env) stms,pos=pos}
fun tie_progenv (env as {varenv,progenv}) = map (tie_progenv' env) progenv

fun tie_env (env as {varenv,progenv}) =
    let val progenv' = tie_progenv {varenv=varenv,progenv=progenv}   (* place the references *)
        val progenv'' = tie_progenv {varenv=varenv,progenv=progenv'} (* replace the references *)
    in {varenv=varenv,progenv=progenv''}
    end

fun update_var(env as {varenv: varenv,progenv: progenv},
               name: S.symbol,sub: W.word,v: W.word): env =
    let fun f array = Array.update(array,W.toIntX sub,v)
            handle Subscript => Err.fatalError 0 "Subscript 2"
        val varenv' = f (valOf (Map.find (varenv,name)))
    in env (* updated *)
    end

fun reset_varenv (env as {varenv,progenv},name) =
    let val _ = case Map.find (varenv,name)
                 of SOME array => Array.modify (fn _ => 0w0: W.word) array
                  | NONE => Err.fatalError 0 "no such variable in reset_varenv"
    in env
    end
fun print_var (s: S.symbol,ws) : unit =
    let val max = Array.length ws
        fun p n (s,ws) = if max = n then ()
                         else let val w' = Array.sub(ws,n)
                              in (print (S.name(s) ^ "[" ^ Int.toString n ^ "]\t" ^ "0x" ^ W.toString w' ^
                                         "\t(" ^ LargeInt.toString (W.toLargeIntX w') ^ ")\n");
                                  p (n+1) (s,ws))
                              end
    in p 0 (s,ws)
    end

fun print_vars ({varenv,progenv},s') =
    case Map.find (varenv,s')
     of SOME array => print_var(s',array)
      | NONE => Err.fatalError 0 "in print_vars"
fun print_allvars {varenv,progenv} = app print_var (Map.listItemsi varenv)

(* keep the initial state of environment, to recover it later *)
val initial_env: env ref = ref {varenv=Map.empty,progenv=[]}
fun set_initial_env (env: env): unit =
    initial_env := (tie_env env
                    handle Error => (print "Note currently env is incomplete!\n"; env))
fun get_initial_env () = !initial_env

fun program2env (A.Program(ls,ps)): env =
    let fun trans (A.Lvalue{name,sub=A.IntExp(m),pos,valarray}) =
            (name,
             Array.array(W.toIntX(m),0w0: W.word)
             handle Overflow => Err.fatalError 0 "overflow2 in program2env"
                  | Subscript => Err.fatalError 0 "Subscript 3")
          | trans _ = Err.impossible "in program2env"
    in {varenv=foldr Map.insert' Map.empty (map trans ls),progenv=ps}
    end

fun elem (x,[]) = false
  | elem (x:S.symbol,y::ys) = if S.eq(x,y) then true
                                   else elem (x,ys)

fun vars (A.VarExp (A.Lvalue {name,...})) = [name]
  | vars (A.IntExp _) = []
  | vars (A.OpExp {left,right,...}) = vars left @ vars right

fun isIncludedIn (name,exp) = elem (name,vars exp)

fun typecheck {varenv,progenv} =
    let (* varenv must not have duplicated entry *)
        fun c_stm (A.ModStm{lvalue=A.Lvalue{name,sub,pos=pos1,valarray},modoper,exp,pos=pos2}) =
            if isIncludedIn(name,exp)
            then ErrorMsg.fatalError pos2 ("Variable " ^ S.name(name) ^ "appeared in RHS")
            else if isIncludedIn(name,exp)
            then ErrorMsg.fatalError pos1 ("Variable " ^ S.name(name) ^ "appeared in the index")
            else ()
          | c_stm (A.IfStm{test1,then',else',test2,pos}) =
            let val _ = Option.map (map c_stm) then'
                val _ = Option.map (map c_stm) else'
            in ()
            end
          | c_stm (A.DoStm{test1,do',loop,test2,pos}) =
            let val _ = Option.map (map c_stm) do'
                val _ = Option.map (map c_stm) loop
            in ()
            end
          | c_stm _ = ()
        fun c_proc (A.Procedure{name,stms,pos}) = app c_stm stms
    in app c_proc progenv
    end

end (* structure Env *)
