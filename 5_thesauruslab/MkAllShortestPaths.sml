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
              val outpair = (v,unvisneibor)
            in outpair end
          fun getunvis (a,b) = b
          fun getparen (a,b) = a
          val info = Seq.map outNeibor frontier
          fun getpair next  = Seq.map (fn s => (s, getparen next)) (getunvis next)
          val parentpair =Table.collect (Seq.flatten (map getpair info))
          val new_asp  = Table.merge (fn (a, b) => a) (reasp,parentpair)
          val new_frontier= Set.toSeq (Table.domain parentpair)
        in
          bfs new_asp new_frontier
        end
    in
      bfs (Table.singleton(v,Seq.singleton(v))) (Seq.singleton(v):vertex seq)
    end

  (* Task 2.5 *)
fun report (A : asp) (v : vertex) : vertex seq seq =
    case Table.find A v of NONE => Seq.empty()
    |_ => 
    let
        fun decide v = let val SOME inv = Table.find A v in inv end
        fun path v = 
        if (Seq.length (decide v))=1 andalso Key.equal(v,nth (decide v) 0) then Seq.singleton(Seq.singleton(v))  
        else let
          val inVseq = decide v
          val ininVseq = Seq.map path inVseq
          val pathx = Seq.map (fn u => (Seq.flatten u)) ininVseq
          val pathxx = Seq.map (fn u => (Seq.append(u,Seq.singleton(v)))) pathx
        in
          pathxx
        end
    in
       path v
    end
end
