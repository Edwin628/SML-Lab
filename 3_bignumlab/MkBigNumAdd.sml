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
      fun fixlen(x,y) = if length(x)>length(y) then (x,(append (y, tabulate (fn i => ZERO) (length(x)-length(y)-1))))
              else if length(x)<length(y) then (append(x,tabulate (fn i => ZERO) (length(y)-length(x)-1)),y)
              else (x,y)
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
      fun getResult init1 =
        let
          fun compare (x,y)=
          case (x,y) of (_,GEN) => GEN
          |((STOP|PROP),PROP) => PROP
          |(GEN,PROP) => GEN
          |(GEN,STOP) => PROP
          |((STOP|PROP),STOP) => STOP
        in
          scani compare STOP init1
        end
      val init2 = getResult init1 
      fun trasnlateResult i = 
        let
          fun translate (GEN|STOP) = ZERO
          |translate PROP = ONE
        in
          translate(nth init2 i)
        end
    in
      if nth init2 (length(init2)-1) = GEN then append(tabulate trasnlateResult (length(init2)-1),singleton ONE)
      else tabulate trasnlateResult (length(init2)-1)
    end
    
  val add = op++
end
