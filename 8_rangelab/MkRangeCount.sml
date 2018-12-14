functor MkRangeCount(structure OrdTable : ORD_TABLE) : RANGE_COUNT =
struct
  structure OrdTable = OrdTable
  open OrdTable

  (* Ordered table type: *)
  type 'a table = 'a table
  type 'a seq = 'a Seq.seq
  type point = Key.t * Key.t

  (* Use this to compare x- or y-values. *)
  val compareKey : (Key.t * Key.t) -> order = Key.compare

  (* Define this yourself *)
  type countTable = Key.t table table

  (* Remove this line before submitting! *)
  exception NYI

  fun makeCountTable (S : point seq) : countTable =
      if Seq.length S = 0 then empty()
      else let
        fun comparex ((x1,y1),(x2,y2))=compareKey(x1,x2)
        val SortedS = Seq.sort comparex S
        val xValue = Seq.map (fn (x,y) => x) SortedS
        val yValue = Seq.map (fn (x,y) => (y,y)) SortedS
        (*fun formyValue i = Seq.subseq yValue (0,i+1)
        val yValue2 = Seq.tabulate formyValue Seq.length(yValue)*)
        fun combine (a,b) = OrdTable.insert (fn (_,x)=>x) b a
        val (yValue2,last) = Seq.iterh combine (empty()) yValue
        val yValue3 = Seq.append (Seq.drop(yValue2,1),Seq.singleton(last))
        val result = Seq.zip xValue yValue3
      in
        OrdTable.fromSeq result
      end
    
  fun count (T : countTable) ((xLeft, yHi) : point, (xRght, yLo) : point) : int  =
    let
        fun insideRec x = size(getRange x (yLo,yHi))     
        fun getinfoL T x = let
          val (l,opt,r) = split(T,x) 
          val leftnum = case last l of SOME (_, thing)  => insideRec thing
           | NONE => 0 
          in
          leftnum
          end
        fun getinfoR T x = let 
           val (l,opt,r) = split(T,x) 
           val rightnum= case opt of SOME thing => insideRec thing
           | NONE => case last l of NONE => 0
                    | SOME (_, thing) => insideRec thing 
          in
          rightnum
          end   
        val leftnum = getinfoL T xLeft
        val rightnum = getinfoR T xRght
    in
        rightnum - leftnum 
    end
end