Tester.testFirst (); 
Tester.testLast (); 
Tester.testPrev (); 
Tester.testNext (); 
Tester.testJoin (); 
Tester.testSplit (); 
Tester.testRange (); 
Tester.testCount ();

CM.make "sources.cm"; 
OS.FileSys.chDir("C:\\Users\\0MEN\\Desktop\\Lab\\8_rangelab");


  fun last (T : 'a table) : (key * 'a) option =
        case Tree.expose T
          of LEAf => NONE
           | NODE of SOME {key, value, left, right} => 
           if right = LEAF then SOME (key,value) else last right


  case find T x of  SOME _ => let val SOME thing = find T x in thing end
          | NONE => let 
          val (l,_,r) = split(T,x) 
          (*val (_,SOME thing) = last r in thing end*)
-----the second way to implement the algorithm-----
        val yValue = Seq.map (fn (x,y) => (y,y)) SortedS
        fun combine (a,b) = OrdTable.insert (fn (x,y)=>x) b a
        val (yValue2,last) = Seq.iterh combine (empty()) yValue
        val yValue3 = Seq.append (Seq.drop(yValue2,1),Seq.singleton(last))
        val yValue2 = Seq.tabulate (fn i => Seq.iter combine (empty()) (Seq.take (yValue, i))) (Seq.length yValue+1)
        val yValue3 = Seq.drop(yValue2,1)

        in
        OrdTable.fromSeq (Seq.zip xValue yValue3)
----the second way to implement the algorithm-----


(*fun getinfoL T x = let
          val (l,opt,r) = split(T,x) 
          val (leftnum,leftdian) = case opt of SOME thing => (insideRec thing,1)
           | NONE => case last l of NONE => (0,0) 
                     | SOME (_, thing) => (insideRec thing,0)
          in
          (leftnum,leftdian)
          end*)

  fun getinfoL T x = let 
           val (l,opt,r) = split(T,x) 
           val leftnum = case opt of SOME thing => thing
           | NONE => case last l of NONE =>empty()
                    | SOME (_, SOME thing)=> thing
          in
          leftnum 
          end
  fun getinfoR T x = let 
           val (l,opt,r) = split(T,x) 
           val rightnum = case opt of SOME thing => thing
           | NONE => case last l of NONE =>empty()
                    | SOME (_, SOME thing) => thing 
          in
          rightnum 
          end   

