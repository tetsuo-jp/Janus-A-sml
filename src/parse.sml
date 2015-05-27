structure Parse
  : sig
      val parse : string -> Absyn.program
    end =
struct

 (* building and linking the lexer and parser *)
  structure JANUSLrVals = JANUSLrValsFun(structure Token = LrParser.Token)
  structure Lex = JANUSLexFun(structure Tokens = JANUSLrVals.Tokens)
  structure JANUSP = Join(structure ParserData = JANUSLrVals.ParserData
                          structure Lex=Lex
                          structure LrParser = LrParser)

  (* parse : string -> Absyn.program *)
  fun parse filename =
      let val _ = ErrorMsg.reset(filename)
          val file = TextIO.openIn filename
              handle Io => (print ("no such file or directory: "^filename^"\n");
                            raise Io)
          fun get _ = TextIO.input file
          fun parseerror(s,p1,p2) = ErrorMsg.error p1 s
          val lexer = LrParser.Stream.streamify (Lex.makeLexer get)
          val (absyn, _) = JANUSP.parse(30,lexer,parseerror,())
       in TextIO.closeIn file;
          absyn
      end handle LrParser.ParseError => raise ErrorMsg.Error

end (* structure Parse *)
