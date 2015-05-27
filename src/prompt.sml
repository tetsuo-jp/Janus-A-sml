(* prompt.sml *)

structure Prompt =
struct

structure L = JanusPrompt.UserDeclarations
structure E = Env
structure W = LargeWord
structure S = Symbol
structure Err = ErrorMsg

val lexer = JanusPrompt.makeLexer( fn n => valOf (TextIO.inputLine( TextIO.stdIn )) )
fun lex_line () =
    let fun f rs =
        case lexer() of
            L.NEWLINE => rs
          | s => f (s :: rs)
    in rev (f [])
    end

fun loadfile file: E.env = E.program2env(Parse.parse file)

(* timer function (not correctly worked in windows) *)
fun time f arg =
    let val timer = Timer.startCPUTimer()
        val real_timer = Timer.startRealTimer()
        val result = f arg
    in result before
       (print (Real.toString(Time.toReal(#usr(Timer.checkCPUTimer timer))));
        print " sec.  Real: ";
        print (Real.toString(Time.toReal(Timer.checkRealTimer real_timer)));
        print " sec.\n")
    end

val t_time = ref false          (* flag: whether we use timer *)

fun top_loop env =
    case (print "> "; lex_line())
     of [] => top_loop env
      | [L.VAR l,L.LBRACKET,L.NUM i,L.RBRACKET] =>
        let val w = E.look_var(env,S.symbol l,W.fromInt i)
        in (print (l ^ "[" ^ Int.toString i ^ "]\t" ^ "0x" ^ W.toString w ^
                   "\t(" ^ LargeInt.toString (W.toLargeIntX w) ^ ")\n");
            top_loop env)
        end
      | [L.VAR l] =>
        (E.print_vars(env,S.symbol(l))
         handle Err.Error => print "No such variable\n";
         top_loop env)
      | [L.VAR l,L.EQ,L.NUM i] =>
        let val env' = E.update_var(env,S.symbol(l),0w0,W.fromInt i)
                  handle Err.Error => (print "No such variable\n"; env)
        in top_loop env' end
      | [L.VAR l,L.LBRACKET,L.NUM i,L.RBRACKET,L.EQ,L.NUM n] =>
        let val env' = E.update_var(env,S.symbol(l),W.fromInt i,W.fromInt n)
                  handle Err.Error => (print "No such variable or out-of-bound\n"; env)
        in top_loop env' end
      | [L.CALL,L.VAR l] =>
        let val _ = E.typecheck env
            val env' = (if !t_time then time else fn x => x)
                           (Eval.call
                                (env,S.symbol l))
                           (ref (SOME (E.look_proc(env,S.symbol l))))
                  handle Err.Error => (print "Runtime error\n"; env)
                       | Err.Dump => (print "Illegal function call\n"; env)
                       | Err.NoProc => (print "No such procedure\n"; env)
                       | Io => (print "IO error in Prompt.call\n"; env)
        in top_loop env' end
      | [L.UNCALL,L.VAR l] =>
        let val _ = E.typecheck env
            val env' = (if !t_time then time else fn x => x)
                           (Eval.uncall
                                (env,S.symbol l))
                           (ref (SOME (E.look_proc(env,S.symbol l))))
                  handle Err.Error => (print "Runtime error\n"; env)
                       | Err.Dump => (print "Illegal function call\n"; env)
                       | Err.NoProc => (print "No such procedure\n"; env)
                       | Io => (print "IO error in Prompt.uncall\n"; env)
        in top_loop env' end
      | [L.SYMBOLS] =>
        (E.print_allvars env; top_loop env)
      | [L.TRACE] =>
        ((if !Eval.trace then print "TRACE alreadly turned-on\n"
          else (Eval.trace := true; print "Set TRACE on\n"));
         top_loop env)
      | [L.UNTRACE] =>
        ((if !Eval.trace then (Eval.trace := false; print "set TRACE off\n")
          else print "TRACE alreadly turned-off\n"); top_loop env)
      | [L.RESET] => top_loop (E.get_initial_env())
      | [L.QUIT] => ()
      | [L.RESET,L.VAR l] => top_loop(E.reset_varenv(env,S.symbol(l)))
      | [L.TIME] =>
        ((if !t_time
          then print "unset TIMER\n"
          else print "set TIMER\n");
         t_time := not (!t_time); top_loop env)
      | [L.ADD,L.STRING filename] =>
        (let val {varenv=varenv1,progenv=progenv1} = E.get_initial_env ()
             val {varenv=varenv2,progenv=progenv2} = loadfile(filename)
             (* the latter file has precedence. *)
             val env' = {varenv=E.append_varenv (varenv1,varenv2),
                         progenv=progenv1 @ progenv2}
             val _ = E.set_initial_env(env')
         in top_loop (E.get_initial_env ())
         end
         handle Io => (print ("no such file: " ^ filename ^ "\n"); top_loop env))
      | ss => (Err.error 0 "Invalid command"; top_loop env)

(* The file name is passed through the argument *)
fun start' file' : unit =
    let val env = loadfile(file')
        val _ = E.set_initial_env env
    in top_loop (E.get_initial_env())
    end

(* root program *)
fun start () : unit =
    let val _ = print "file? "
        val file = valOf(TextIO.inputLine( TextIO.stdIn ))
        val file' = String.substring(file,0,size file-1) (* chop the newline char *)
    in start' file' end

end (* structure Prompt *)
