functor MkBigNumSubtract(structure BNA : BIGNUM_ADD) : BIGNUM_SUBTRACT =
struct
  structure Util = BNA.Util
  open Util
  open Seq

  exception NotYetImplemented
  infix 6 ++ --
  fun x ++ y = BNA.add (x, y)
  fun x -- y =
    let
      val lenx = length(x)
      val leny = length(y)
      val len = if lenx>leny then lenx else leny
      (*function fixlen is used to make two sequence the same length*)
      fun fixlen(x,y) = if lenx>leny then (x,(append (y, tabulate (fn i => ZERO) (lenx-leny))))
              else if lenx<leny then (append(x,tabulate (fn i => ZERO) (leny-lenx)),y)
              else (x,y)
      val (x,y)=fixlen(x,y)
      val (x,fixone)=fixlen(x,singleton ONE)
      (*function reverse used the knowledge of complement representation.
      In order to get the opposite value of a sequence, we need to flip all the
      bits, and then add one to the bit sequence*)
      fun reverse y = 
        let 
          fun reversedigit i =
            if nth y i = ONE then ZERO
            else ONE
        in
        tabulate reversedigit len
        end
    in
      take(x ++ reverse(y) ++ singleton ONE, len)
    end
      
  val sub = op--
end
