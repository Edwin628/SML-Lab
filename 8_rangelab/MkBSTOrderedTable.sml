functor MkBSTOrderedTable (structure Tree : BSTREE
                           structure Seq : SEQUENCE) : ORD_TABLE =
struct

  structure Table = MkBSTTable(structure Tree = Tree
                               structure Seq = Seq)

  (* Include all the functionalities of the standard Table *)
  open Table

  (* This is defined after "open" so it doesn't get overwritten *)
  structure Key = Tree.Key
  type key = Key.t

  (* Remember, type 'a table = 'a Tree.bst *)


  fun first (T : 'a table) : (key * 'a) option =
        case expose T
          of NONE => NONE
           | SOME {key, value, left, right} => 
           if left = NONE then SOME (key,value) else first left

  fun last (T : 'a table) : (key * 'a) option =
        case expose T
          of NONE => NONE
           | SOME {key, value, left, right} => 
           if right = NONE then SOME (key,value) else last right
		      
  fun previous (T : 'a table) (k : key) : (key * 'a) option =
    raise NYI

  fun next (T : 'a table) (k : key) : (key * 'a) option =
    raise NYI

  fun join (L : 'a table, R : 'a table) : 'a table =
    raise NYI

  fun split (T : 'a table, k : key) : 'a table * 'a option * 'a table =
    raise NYI

  fun getRange (T : 'a table) (low : key, high : key) : 'a table =
    raise NYI
						       

end
