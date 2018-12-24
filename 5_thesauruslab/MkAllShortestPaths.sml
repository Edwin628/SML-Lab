functor MkAllShortestPaths (Table : TABLE) : ALL_SHORTEST_PATHS =
struct
  open Table
  open Seq

  (* Table.key defines our vertex type *)
  type vertex = key
  type edge = vertex * vertex

  exception NYI
  (* You must define the following two types and
   * explain your decision here with comments.
   *)
  type graph = vertex seq table*int
  type asp = vertex seq table

  (* Task 2.1 *)
  fun makeGraph (E : edge seq) : graph =
        if Seq.length E = 0 then (Table.empty(),0)
        else  let 
                val formoutedge = Table.collect E
                val edgenum = Seq.length E
              in
                (formoutedge,edgenum)
              end

  (* Task 2.2 *)
  fun numEdges (G : graph) : int = #2 G

  fun numVertices (G : graph) : int = 
        let 
          val (formoutedge,_)=G
          val E2 = Table.map Set.fromSeq formoutedge
          val vertinfo = Table.reduce (Set.union) (Table.domain formoutedge) E2
          val Outvertice = Set.union ((Table.domain formoutedge),vertinfo)
          val vertnum = Set.size(Outvertice)
        in
          vertnum
        end

  (* Task 2.3 *)
  fun outNeighbors (G : graph) (v : vertex) : vertex seq =
      let
        val (Gra,_)=G
        val outver = case find Gra v of NONE => Seq.empty()
          |SOME outver => outver
      in
        outver
      end
      
             
  (* Task 2.4 *)
  fun makeASP (G : graph) (v : vertex) : asp =
    let
      fun bfs (reasp:asp) (frontier:vertex seq) =
        if Seq.length frontier = 0 then reasp
        else let
          fun outNeibor v = 
            let 
              val neibor = outNeighbors G v
              val unvisneibor = Seq.filter (fn v =>not (Set.find (Table.domain reasp) v)) neibor
              val parentpair =Seq.map (fn u =>(u,v)) unvisneibor
              val outpair = (parentpair,unvisneibor)
            in outpair end
          fun getunvis (a,b) = b
          fun getparen (a,b) = a
          val info = Seq.map outNeibor frontier
          val new_asp  = Table.merge (fn (a, b) => a) (reasp,(Table.collect Seq.flatten(Seq.map getparen info)))
          val new_frontier =Seq.flatten (Seq.map getunvis info)
        in
          bfs new_asp new_frontier
        end
    in
      bfs (Table.singleton(v,Seq.singleton(v))) Seq.singleton(v)
    end

  (* Task 2.5 *)
fun report (A : asp) (v : vertex) : vertex seq seq =
  let
      fun find_parent x = getOpt(Table.find A x, Seq.empty ())
      (*v_asp : compare 2 vertices*)
      fun v_cmp (a : vertex, b : vertex) = 
        if Table.Key.equal (a, b) then EQUAL else LESS
      (*del_rep : delete repeated paths*)
      fun del_rep (s : vertex seq seq) = 
        map (fn p => #1 p) (Seq.collect (collate v_cmp) 
          (zip s (tabulate (fn _ => ()) (length s))))
      fun make_path v : vertex seq seq =
        if length (find_parent v) = 0 then empty()
        else if v_cmp (nth (find_parent v) 0, v) = EQUAL then %[%[v]]
        else let
          val nexts_data = find_parent v
          val nexts : vertex seq seq seq = map make_path nexts_data
        in
          flatten (map (fn x => map (fn y => append (y, %[v])) x) nexts)
        end
    in
      del_rep(make_path v)
    end
end
