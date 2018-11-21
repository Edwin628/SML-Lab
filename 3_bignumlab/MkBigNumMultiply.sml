functor MkBigNumMultiply(structure BNA : BIGNUM_ADD
                         structure BNS : BIGNUM_SUBTRACT
                         sharing BNA.Util = BNS.Util) : BIGNUM_MULTIPLY =
struct
  structure Util = BNA.Util
  open Util
  open Seq
  open Primitives
  exception NotYetImplemented

  infix 6 ++ --
  infix 7 **

  fun x ++ y = BNA.add (x, y)
  fun x -- y = BNS.sub (x, y)
  val len = if length(x)>length(y) then length(x) else length(y)
  fun x ** y = 
      if len =0 then empty()
      else if len = 1 andalso (nth x 0 = ZERO orelse nth y 0 = ZERO) then empty()
      else if len = 1 andalso nth x 0= ONE andalso nth y 0= ONE then singleton(ONE)
    else
    let
      val len = if length(x)>length(y) then length(x) else length(y)
      fun fixlen(x,y) = if length(x)>length(y) then (x,(append (y, tabulate (fn i => ZERO) (length(x)-length(y)))))
              else if length(x)<length(y) then (append(x,tabulate (fn i => ZERO) (length(y)-length(x))),y)
              else (x,y)
      val (x,y) = fixlen(x,y)
      val (p,q) = (take(x,length(x) div 2),drop(x,length(x) div 2))
      val (r,s) = (take(x,length(x) div 2),drop(x,length(x) div 2))
      val pr = p ** r
      val qs = q ** s
      val pqrs = (p ++ q) ** (r ++ s)- pr - qs
      fun transfer (chuan,num) = append((tabulate (fn i => ZERO) num) ,chuan)
      val npr = transfer(pr,len)
      val npqrs = transfer(pqrs,len div 2)
    in
      npr ++ npqrs ++ qs
    end
  val mul = op**
end
