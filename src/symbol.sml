(* symbol.sml *)

structure Symbol :> SYMBOL =
struct

  type symbol = string * int

  structure H = HashTable

  exception Symbol
  val nextsym = ref 0
  val sizeHint = 128
  val hashtable : (string,int) H.hash_table =
      H.mkTable(HashString.hashString, op = ) (sizeHint,Symbol)

  fun symbol name =
      case H.find hashtable name
       of SOME i => (name,i)
        | NONE => let val i = !nextsym
                   in nextsym := i+1;
                      H.insert hashtable (name,i);
                      (name,i)
                  end

  fun name(s,n) = s

  fun eq((_,n1): symbol, (_,n2): symbol) = (n1 = n2)
  fun compare((_,n1): symbol, (_,n2): symbol) = Int.compare(n1,n2)

  fun gt((s1,_): symbol, (s2,_): symbol) = (s1 > s2)

  structure Table = IntMapTable(type key = symbol
                                fun getInt(s,n) = n)

  type 'a table= 'a Table.table
  val empty = Table.empty
  val enter = Table.enter
  val enter' = Table.enter'
  val look = Table.look

end (* structure Symbol *)
