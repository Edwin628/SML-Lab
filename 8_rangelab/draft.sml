Tester.testFirst (); 
Tester.testLast (); 
Tester.testPrev (); 
Tester.testNext (); 
Tester.testJoin (); 
Tester.testSplit (); 
Tester.testRange (); 
Tester.testCount ();

CM.make "sources.cm"; 
OS.FileSys.chDir("C:\\Users\\0MEN\\Desktop\\Lab\\8_rangelab");


  fun last (T : 'a table) : (key * 'a) option =
        case Tree.expose T
          of LEAf => NONE
           | NODE of SOME {key, value, left, right} => 
           if right = LEAF then SOME (key,value) else last right