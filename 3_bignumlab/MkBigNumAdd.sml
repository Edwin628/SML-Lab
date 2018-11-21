functor MkBigNumAdd(structure U : BIGNUM_UTIL) : BIGNUM_ADD =
struct
  structure Util = U
  open Util
  open Seq

  infix 6 ++
  exception NotYetImplemented
  datatype carry = GEN | PROP | STOP

  fun x ++ y =
    let
      val len = if length(x)>length(y) then length(x) else length(y)
      fun fixlen(x,y) = if length(x)>length(y) then (x,(append (y, tabulate (fn i => ZERO) (length(x)-length(y)))))
              else if length(x)<length(y) then (append(x,tabulate (fn i => ZERO) (length(y)-length(x))),y)
              else (x,y)
      val (x,y)=fixlen(x,y)
      fun addx (x,y) =
        let 
          fun addxx i =
          case (nth x i,nth y i) of (ZERO,ZERO)=>STOP
          |((ZERO,ONE)|(ONE,ZERO))=>PROP
          |(ONE,ONE) => GEN
        in
          tabulate addxx len
        end
      val init1 = addx (x,y) 
      fun getSomething init =
        let
          fun carry (x,y)=
          case (x,y) of (_,GEN) => GEN
          |((STOP|PROP),PROP) => PROP
          |(GEN,PROP) => GEN
          |(GEN,STOP) => PROP
          |((STOP|PROP),STOP) => STOP
        in
           scan carry STOP init
        end
      val (carryseq,last) = getSomething init1
      fun compareResult i = 
        let
          fun compare (l,t)=
          case (l,t) of (GEN,GEN) => ONE
          |(STOP,GEN) => ONE
          |(PROP,PROP) => ONE
          |(PROP,STOP) => ONE
          |_ => ZERO
        in
          compare(nth init1 i,nth carryseq i)
        end
    in
      if last = GEN then append(tabulate compareResult (length(init1)),singleton ONE)
      else tabulate compareResult (length(init1))
    end
  val add = op++
end
