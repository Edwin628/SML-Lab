functor MkBSTOrderedTable (structure Tree : BSTREE
                           structure Seq : SEQUENCE) : ORD_TABLE =
struct

  structure Table = MkBSTTable(structure Tree = Tree
                               structure Seq = Seq)

  (* Include all the functionalities of the standard Table *)
  open Table
  open Tree
  (* This is defined after "open" so it doesn't get overwritten *)
  structure Key = Tree.Key
  type key = Key.t
  exception NYI
  (* Remember, type 'a table = 'a Tree.bst *)


  fun first (T : 'a table) : (key * 'a) option =
        case expose T
          of NONE => NONE
           | SOME {key, value, left, right} => 
            case expose left of NONE => SOME (key,value) 
            | _ => (first left)

  fun last (T : 'a table) : (key * 'a) option =
        case expose T
          of NONE => NONE
           | SOME {key, value, left, right} => 
            case expose right of NONE => SOME (key,value)
            | _ => (last right)
		      
  fun previous (T : 'a table) (k : key) : (key * 'a) option =
      let val (ltree,b,rtree)=splitAt (T,k) in last ltree end

  fun next (T : 'a table) (k : key) : (key * 'a) option =
      let val (ltree,b,rtree)=splitAt (T,k) in first rtree end

  fun join (L : 'a table, R : 'a table) : 'a table =
      Tree.join (L,R)
  fun split (T : 'a table, k : key) : 'a table * 'a option * 'a table =
      splitAt (T,k)

  fun getRange (T : 'a table) (low : key, high : key) : 'a table =
    raise NYI
						       

end
