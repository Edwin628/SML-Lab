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
      val len = if length(x)>length(y) then length(x) else length(y)
      fun fixlen(x,y) = if length(x)>length(y) then (x,(append (y, tabulate (fn i => ZERO) (length(x)-length(y)))))
              else if length(x)<length(y) then (append(x,tabulate (fn i => ZERO) (length(y)-length(x))),y)
              else (x,y)
      val (x,y)=fixlen(x,y)
      val (x,fixone)=fixlen(x,singleton ONE)
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
