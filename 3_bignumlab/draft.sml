

OS.FileSys.chDir("C:\\Users\\0MEN\\Desktop\\Lab\\3_bignumlab");



- CM.make "sources.cm"; ... - Tester.testBF (); ... - Tester.testDC (); ...

CM.make "sources.cm"; Tester.testAdd (); Tester.testSub (); Tester.testMul ();


      fun addZero (t,i) = nth t (length(t)+i) = ZERO
      fun addZero (t,i) = nth t (length(t)+i) = ZERO
      fun fixlen(x,y) = if length(x)>length(y) then (x,append(y,tabulate (fn i=>addZero(y,i)) (length(x)-length(y)-1)))
              else if length(x)<length(y) then (append(x,tabulate (fn i=>addZero(x,i)) (length(y)-length(x)-1)),y)
              else (x,y)



fun copy (c1,c2) =
         if c2 = PROP then c1 else c2
        val (rst,last) = scan copy STOP xy   (*rst contains only GEN and STOP*)
        fun calc ((STOP|GEN), c) = if c = GEN then ONE else ZERO
          | calc (PROP,c)        = if c = GEN then ZERO else ONE
        val rst' = map2 calc xy rst        
      in
        if last = GEN then append (rst',singleton ONE)
        else rst'
      end
        val pr : bit seq = multiply(p,r)
        val qs : bit seq = multiply(q,s)
        val pqrs: bit seq = multiply(ppq,rps) -- pr -- qs
        val (pr,qs,pqrs)= par3(fn f => multiply(p,r),fn f =>multiply(q,s),fn f =>multiply(ppq,rps))
if length(x)=0 orelse length(y) = 0 then empty()
      else if (length(x)=1 orelse length(y)=1) andalso (nth x 0 = ZERO orelse nth y 0 = ZERO) then empty()
      else if (length(x)=1 orelse length(y)=1) andalso nth x 0= ONE andalso nth y 0= ONE then singleton(ONE)


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
      (*firstly we need to decide some base case*)
      if length(x)=0 orelse length(y) = 0 then empty()
      else if (length(x)=1 andalso nth x 0 = ZERO) orelse (length(y)=1 andalso nth y 0 = ZERO) then empty()
      else if (length(x)=1 andalso length(y)=1) andalso nth x 0= ONE andalso nth y 0= ONE then singleton(ONE)
    else
      let
        val lenx = if length(x)>length(y) then length(x) else length(y)
        fun fixlen(x,y) = if length(x)>length(y) then (x,(append (y, tabulate (fn i => ZERO) (length(x)-length(y)))))
              else if length(x)<length(y) then (append(x,tabulate (fn i => ZERO) (length(y)-length(x))),y)
              else (x,y)
        val (x,y) = fixlen(x,y)
        val (q,p) = (take(x,length(x) div 2),drop(x,length(x) div 2))
        val (s,r) = (take(y,length(y) div 2),drop(y,length(y) div 2))
        val ppq =p ++ q
        val rps =r ++ s
        val pr : bit seq = multiply(p,r)
        val qs : bit seq = multiply(q,s)
        val pqrs: bit seq = multiply(ppq,rps) -- pr -- qs
        fun transf (num,chuan) = append(tabulate (fn _ => ZERO) (num),chuan)
        val npr: bit seq = transf(2*(lenx div 2),pr)
        val npqrs: bit seq = transf(lenx div 2,pqrs)
      in
        npr ++ npqrs ++ qs
      end
    in
      multiply(x,y)
    end
  val mul = op**
end
