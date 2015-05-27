(* symbol.sig *)

signature SYMBOL =
sig

  type symbol
  val symbol : string -> symbol
    (* create a unique symbol from a string *)
  val name : symbol -> string
    (* return the name of a symbol -- the string from which the
     * symbol was created *)
  val eq : symbol * symbol -> bool
    (* eq(s1,s2) is true if s1 and s2 are the same symbol;
     * two symbols with the same name will be equal *)
  val compare : symbol * symbol -> order
  val gt : symbol * symbol -> bool
    (* gt(s1,s2) is true if s1 > s2 in alphabetical ordering,
     * using the ascii collating sequence *)

  structure Table : TABLE where type key = symbol

  type 'a table
    (* functional symbol tables, parameterized by binding type *)
  val empty : 'a table
    (* empty symbol table; starting point for building tables by
     * adding bindings *)
  val enter : 'a table * symbol * 'a -> 'a table
    (* add a symbol binding to a table, producing a new table.  The
     * original table is not modified *)
  val look  : 'a table * symbol -> 'a option
    (* lookup(t,s) : returns (SOME b), where b is the binding associated
     * with symbol s in table t, if it exists; otherwise returns NONE *)

end (* signature SYMBOL *)
