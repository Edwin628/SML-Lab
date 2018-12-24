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
  type asp = unit

  (* Task 2.1 *)
  fun makeGraph (E : edge seq) : graph =
        if Seq.length E = 0 then (Table.empty(),0)
        else  let 
                val formoutedge = Table.collect E
                val edgenum = Seq.length E
                (*fun changeAB (a,b) = (b,a)
                val E2 = Seq.map changeAB E
                val forminedge = Table.collect E2*)
                
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
  raise NYI

  (* Task 2.5 *)
fun report (A : asp) (v : vertex) : vertex seq seq =
 raise NYI

end
