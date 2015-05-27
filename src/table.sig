signature TABLE =
sig
   type key
   type 'a table
   val empty : 'a table
   val enter : 'a table * key * 'a -> 'a table
   val enter' : (key * 'a) * 'a table -> 'a table
   val look  : 'a table * key -> 'a option

   (* for debuggin use *)
   val listItems : 'a table -> 'a list
   val listItemsi : 'a table -> (int * 'a) list
end

