(* driver.sml *)

structure Driver
   : sig
       val export_parse : unit -> unit
       val export_janus : unit -> unit
       val export_inverter : unit -> unit
       val export_trans : unit -> unit
     end =
struct

  fun driver_parse (_,files) =
      let val file = List.hd files
          val absyn = Parse.parse file             (* parsing *)
       in PrintAbsyn.print(TextIO.stdOut, absyn);  (* printing absyn *)
          OS.Process.success                       (* return Unix command result *)
      end

  fun driver_prompt (_,nil) = (Prompt.start (); OS.Process.success)
    | driver_prompt (_,file::_) = (Prompt.start' file; OS.Process.success)

  fun driver_inverter (_,files) =
      let val file = List.hd files
          val _ = Invert.invert file
       in OS.Process.success                       (* return Unix command result *)
      end

  (* A Janus program to an encoded Janus program of self-interpreter *)
  fun driver_translator (_,files) =
      let val file = List.hd files
          val _ = Trans.trans file
       in OS.Process.success                       (* return Unix command result *)
      end

  (* exporting the heap image *)
  fun export_parse() = SMLofNJ.exportFn("janus-parse",driver_parse)
  fun export_janus() = SMLofNJ.exportFn("janus",driver_prompt)
  fun export_inverter() = SMLofNJ.exportFn("janus-inv",driver_inverter)
  fun export_trans() = SMLofNJ.exportFn("janus-trans",driver_translator)

end
