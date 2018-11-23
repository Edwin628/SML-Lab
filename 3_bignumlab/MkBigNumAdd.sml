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
      val lenx = length(x)
      val leny = length(y)
      val len = if lenx>leny then lenx else leny
      (*function fixlen is used to make two sequence the same length*)
      fun fixlen(x,y) = if lenx>leny then (x,(append (y, tabulate (fn i => ZERO) (lenx-leny))))
              else if lenx<leny then (append(x,tabulate (fn i => ZERO) (leny-lenx)),y)
              else (x,y)
      val (x,y)=fixlen(x,y)
      (*function addx is used to caculate the sum of each corresponding bit, the result is 
      the carrybit information*)
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
      (*function getSomething is used to decide the pattern of each bit 
      after adding the carrybit *)
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
      (*function compareResult is used to decide the result of each bit is 0 or 1 ,
      notice that if we have GEN type in the most significant bir ,we must make the
      the length of bit sequence plus one*)
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
