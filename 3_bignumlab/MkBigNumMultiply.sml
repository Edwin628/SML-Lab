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

  fun x ** y = 
   let
      fun multiply(x,y) = 
      if length(x)=0 orelse length(y) = 0 then empty()
      else if (length(x)=1 orelse length(y)=1) andalso (nth x 0 = ZERO orelse nth y 0 = ZERO) then empty()
      else if (length(x)=1 orelse length(y)=1) andalso nth x 0= ONE andalso nth y 0= ONE then singleton(ONE)
    else
      let
        val lenx = if length(x)>length(y) then length(x) else length(y)
        fun fixlen(x,y) = if length(x)>length(y) then (x,(append (y, tabulate (fn i => ZERO) (length(x)-length(y)))))
              else if length(x)<length(y) then (append(x,tabulate (fn i => ZERO) (length(y)-length(x))),y)
              else (x,y)
        val (x,y) = fixlen(x,y)
        val (p,q) = (take(x,length(x) div 2),drop(x,length(x) div 2))
        val (r,s) = (take(x,length(x) div 2),drop(x,length(x) div 2))
        val ppq =p ++ q
        val rps =r ++ s
        val pr : bit seq = multiply(p,r)
        val qs : bit seq = multiply(q,s)
        val pqrs: bit seq = multiply(ppq,rps) -- pr -- qs
        fun transf (num,chuan) = append(tabulate (fn _ => ZERO) (num),chuan)
        val npr: bit seq = transf(lenx,pr)
        val npqrs: bit seq = transf(lenx div 2,pqrs)
      in
        npr ++ npqrs ++ qs
      end
    in
      multiply(x,y)
    end
  val mul = op**
end
