
Tester.testNumEdges ();
Tester.testNumVertices (); 
Tester.testOutNeighbors ();
Tester.testReport ();
Tester.testNumWords ();
Tester.testSynonyms ();
Tester.testQuery ();





CM.make "sources.cm"; 


OS.FileSys.chDir("C:\\Users\\0MEN\\Desktop\\Lab\\5_thesauruslab");

------------------------------------------Version#1--------------------------------------------------------------
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

------------------------------------------Version#2--------------------------------------------------------------
  fun makeASP (G : graph) (v : vertex) : asp =
    let
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