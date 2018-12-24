
Tester.testNumEdges ();
Tester.testNumVertices (); 
Tester.testOutNeighbors ();
Tester.testReport ();
Tester.testNumWords ();
Tester.testSynonyms ();
Tester.testQuery ();





CM.make "sources.cm"; 


OS.FileSys.chDir("C:\\Users\\0MEN\\Desktop\\Lab\\5_thesauruslab");



fun makeASP (G : graph) (v : vertex) : asp =
let
(*复杂度分析
因为我们就是走了一个bfs，
对于这个bfs的每个边，只会走两次，所以work就可以保证到 O(|E| log |V |)
同样，span也是满足O(D log2 |V |)
实现思路，
每个点去找它asp上的parents，每次bfs继续走的时候从froniter向外
然后把向外的两边中，除去相回走的那一个frontier，继续向前走一步，
并用map。把frontier里的点来map成从指向的edge。
当 F 为0时,结束。
*)
     fun BFS (parent_edge) (frontier : vertex seq) (Graph:graph) : asp=
     if (Seq.length frontier) = 0 then
       parent_edge
    else
     let
      val p_edges = Table.collect (Seq.flatten ( Seq.map (fn v => (Seq.map (fn u => (u, v)) (quick_outNeighbors (Graph,v)))) frontier))
      val updated_p_edge= Table.merge (fn (a, b) => a) (parent_edge, p_edges)
      val updated_frontier= Set.toSeq (Table.domain (Table.erase (p_edges, Table.domain parent_edge)))
     in
        BFS updated_p_edge updated_frontier Graph
   end
in
 BFS (Table.singleton (v, Seq.empty ())) (Seq.singleton v) G
end