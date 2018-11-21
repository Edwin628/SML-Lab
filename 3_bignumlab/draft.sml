

OS.FileSys.chDir("C:\\Users\\0MEN\\Desktop\\Lab\\3_bignumlab");



- CM.make "sources.cm"; ... - Tester.testBF (); ... - Tester.testDC (); ...

CM.make "sources.cm"; Tester.testAdd (); Tester.testSub (); Tester.testMul ();


      fun addZero (t,i) = nth t (length(t)+i) = ZERO
      fun addZero (t,i) = nth t (length(t)+i) = ZERO
      fun fixlen(x,y) = if length(x)>length(y) then (x,append(y,tabulate (fn i=>addZero(y,i)) (length(x)-length(y)-1)))
              else if length(x)<length(y) then (append(x,tabulate (fn i=>addZero(x,i)) (length(y)-length(x)-1)),y)
              else (x,y)