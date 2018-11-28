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
      (*firstly we need to decide some base case*)
      if length(x)=0 orelse length(y) = 0 then empty()
      else if (length(x)=1 andalso nth x 0 = ZERO) orelse (length(y)=1 andalso nth y 0 = ZERO) then empty()
      else if (length(x)=1 andalso length(y)=1) andalso nth x 0= ONE andalso nth y 0= ONE then singleton(ONE)
    else
      let
        val lenx = if length(x)>length(y) then length(x) else length(y)
        (*function fixlen is used to make two sequence the same length*)
        fun fixlen(x,y) = if length(x)>length(y) then (x,(append (y, tabulate (fn i => ZERO) (length(x)-length(y)))))
              else if length(x)<length(y) then (append(x,tabulate (fn i => ZERO) (length(y)-length(x))),y)
              else (x,y)
        val (x,y) = fixlen(x,y)
        (*Use take and drop to divide the bit sequence into two parts*)
        val (q,p) = (take(x,length(x) div 2),drop(x,length(x) div 2))
        val (s,r) = (take(y,length(y) div 2),drop(y,length(y) div 2))
        (*then use the algorithm in the homework introduction.That is 
        A=p|q,B=r|S,then A·B = pr·2^n +(ps+rq)·2^n/2 +qs *)
        val ppq =p ++ q
        val rps =r ++ s
        val (pr,qs,pqrsx) = par3(fn() => p**r, fn() => q**s ,fn() =>ppq**rps)
        val pqrs: bit seq = pqrsx -- pr -- qs
        (*function transf is used to caculate pr·2^n and (ps+rq)·2^n/2*)
        fun transf (num,chuan) = append(tabulate (fn _ => ZERO) (num),chuan)
        val npr: bit seq = transf(2*(lenx div 2),pr)
        val npqrs: bit seq = transf(lenx div 2,pqrs)
      in
        npr ++ npqrs ++ qs
      end
  val mul = op**
end
