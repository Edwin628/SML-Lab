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
              (*w=O(|E|log|E|) s=O(log^2|E|)*)
                val formoutedge = Table.collect E
              (*w=O(1) s=O(1)*)
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
        (*w=O(log|V|) s=O(log|V|)*)
        val outver = case find Gra v of NONE => Seq.empty()
          |SOME outver => outver
      in
        outver
      end
      
             
  (* Task 2.4 *)
  fun makeASP (G : graph) (v : vertex) : asp =
    let
    (*复杂度分析：这个做表我使用到了bfs的思想，唯一不同的是frontier我选择
    的是内部的是在已访问节点的边界，因为每个节点至多访问一次，每条边至多出现
    一次操作，所以Work操作为O(|E|log|V|)。对span进行分析，因为每轮操作为
    O(log^2|V|),最长的那个长度即为Span的情况，有D轮，所以span为O(Dlog^2|V|)
    *)
      fun bfs (reasp:asp) (frontier:vertex seq)(visited : Set.set) =
        if Seq.length frontier = 0 then reasp
        else let
          fun outNeibor v = 
            let 
              val neibor = outNeighbors G v
              val unvisneibor = Seq.filter (fn v =>not (Set.find visited v)) neibor
              val outpair = (v,unvisneibor)
            in outpair end
          fun getunvis (a,b) = b
          fun getparen (a,b) = a
          val info = Seq.map outNeibor frontier
          fun getpair next  = Seq.map (fn s => (s, getparen next)) (getunvis next)
          val parentpair =Table.collect (Seq.flatten (map getpair info))
          val new_asp  = Table.merge (fn (a, b) => a) (reasp,parentpair)
          val new_frontier= Set.toSeq (Table.domain parentpair)
          val new_visited = Table.domain new_asp
        in
          bfs new_asp new_frontier new_visited
        end
    in
      bfs (Table.singleton(v,Seq.singleton(v))) (Seq.singleton(v):vertex seq) (Set.empty())
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
        (*w=O(log|V|) s=O(log|V|*)
          val inVseq = decide v
          (*共递归小于log|V|层*)
            val ininseq= Seq.map path inVseq
            (*w=O(ni) s=O(1)*)
            val pathx = Seq.flatten ininseq           
            (*w=O(ni) s=O(1)*)
            val pathxx=Seq.map (fn u => Seq.append(u,Seq.singleton(v))) pathx
        in
         pathxx
        end
    in
       path v
    end
end
