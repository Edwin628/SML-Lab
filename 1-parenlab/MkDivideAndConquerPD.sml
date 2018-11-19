functor MkDivideAndConquerPD (structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open Primitives
  open P
  open Seq
  open Option210


  fun parenDist (parens : paren seq) : int option = 
  let
    fun max2 (s,t) = if s > t then s else t
    (*max3 is used to get the longest distance among the three elements *)
    fun max3 (s,t,u) = if u > max2(s,t) then u else max2(s,t) 
    fun Dist s =
    (*The parameter is a five elements tuple,the first Dis_ denotes the 
    Maximum Parenthesis Distance in the current sequence,the second sl_ denotes
    the unmatched left parenthesis,the third sr_ denotes the right parenthesis
    the fourth dl_ denotes the max distance between the unmatched left parenthesis 
    and the ending position. the fifth dr_ denotes the max distance between the 
    unmatched right parenthesis and the starting position*)
        case (showt s) 
          of EMPTY => (0,0,0,0,0)
          |  ELT OPAREN => (0,1,0,1,0)
          |  ELT CPAREN => (0,0,1,0,1)
          |  NODE (l,r) => 
             let 
               val ((Disl,sl1,sr1,dl1,dr1),(Disr,sl2,sr2,dl2,dr2))= par(fn f => Dist l,fn f => Dist r)
               fun merge ((Disl,sl1,sr1,dl1,dr1),(Disr,sl2,sr2,dl2,dr2)) =
                let 
                  val across = if sl1 = sr2 then dl1+dr2 else 0
                in
                  if sl1 > sr2 then (max3(Disl,Disr,across),sl1-sr2+sl2,sr1,dl1+length(r),dr2)
                  else if sl1 = sr2 then (max3(Disl,Disr,across),sl2,sr1,dl2,dl1)
                  else (max3(Disl,Disr,across),sl2,sr2-sl1+sr1,dl2,dr2+length(l))
                end
             in
               merge ((Disl,sl1,sr1,dl1,dr1),(Disr,sl2,sr2,dl2,dr2))
             end
    val (Dis,sl,sr,dl,dr)= Dist parens
    in
      if length(parens)=0 then NONE
      else SOME Dis
    end
end