(* errormsg.sml *)

signature ERRORMSG =
sig
    val anyErrors : bool ref
    val fileName : string ref

    (* The following global variables are refered in lexing *)
    val lineNum : int ref
    val linePos : int list ref

    exception Error
    exception Dump
    exception NoProc

    val error : int -> string -> unit
    val fatalError : int -> string -> 'a
    val impossible : string -> 'a
    val reset : string -> unit
    val unimplemented : string -> 'a
    val noProc : int -> string -> 'a
end

structure ErrorMsg : ERRORMSG =
struct

  val anyErrors = ref false
  val fileName = ref ""
  val lineNum = ref 1     (* number of last line *)
  val linePos = ref [0]   (* list of initial char positions of lines, in reverse order *)

  fun reset(fname: string) =
      (anyErrors:=false;
       fileName:=fname;
       lineNum:=1;
       linePos:=[0])

  exception Error
  exception Dump
  exception NoProc              (* no such procedure *)

  fun printPos pos =
      let fun look(a::rest,n) =
              if a<pos then app print [":",
                                       Int.toString n,
                                       ".",
                                       Int.toString (pos-a)]
              else look(rest,n-1)
            | look _ = print "0.0"
      in look(!linePos, !lineNum)
      end

  fun error pos (msg:string) =
      (anyErrors := true;
       print (!fileName);
       print ":";
       printPos pos;
       print ":"; print msg; print "\n")

  fun fatalError pos (msg:string) =
      (error pos msg;
       raise Error)

  fun impossible msg =
      (app print ["Error: Compiler bug: ",msg,"\n"];
       TextIO.flushOut TextIO.stdOut;
       raise Error)

  fun unimplemented msg =
      (app print ["Error: not implemented: ",msg,"\n"];
       TextIO.flushOut TextIO.stdOut;
       raise Error)

  fun noProc pos (msg:string) =
      (error pos msg;
       raise NoProc)

end  (* structure ErrorMsg *)
