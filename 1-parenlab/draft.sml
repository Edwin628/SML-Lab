fun parenMatch p = if length(p)<=1 then false else if nth p 0 = CPAREN then false else
    let
    fun pm (((NONE,y) , _) | (((SOME 0,y)),_)) = (NONE,y) 
      | pm ((SOME c,y),CPAREN) = (SOME (c-1),y+1) 
      | pm ((SOME c,y), OPAREN) = (SOME (c+1),y+1)
    val c = length p
    in 
    iter pm (SOME 1,1) (subseq p (1,c-1))  = (SOME 0,c) 
    end


OS.FileSys.chDir("C:\\Users\\0MEN\\Desktop\\Lab\\1-parenlab");



- CM.make "sources.cm"; ... - Tester.testBF (); ... - Tester.testDC (); ...


val len = length parens
        fun parenMatch S = if length S <= 1 then false
        else if nth S 0 = CPAREN then false
        else if length S = 2 andalso nth S 1 = OPAREN then false 
        else
          let 
            fun Count(x,p) = 
             let
               lenx = lenghth(p)
             in
              case (x,p) of 
                ((NONE,_),_) => (NONE,_)
              | ((SOME c,i),OPAREN) =>if c=0 andalso i<lenx-1 else (SOME (c+1),i+1)
              | ((SOME c,i),CPAREN) => if (c=0) then (NONE,_) else (SOME (c-1),i+1)
             end
          in
            iter Count (SOME 1,1) S = (SOME 0,length(S))
          end 


          fun parenMatch S = if length S <= 1 then false
        else if nth S 0 = CPAREN then false
        else if length S = 2 andalso nth S 1 = OPAREN then false 
        else
          let 
            fun Count(x,p) = 
              case (x,p) of 
                (NONE,_) => NONE
              | (SOME c,OPAREN) => SOME (c+1)
              | (SOME c,CPAREN) => if (c=0) then NONE else SOME (c-1)
          in
            iter Count SOME 1 (subseq parens (1,length(parens)-2)) = SOME 0
          end 
两个示例得不到
 fun parenMatch S = if length S <= 1 then false
        else if nth S 0 = CPAREN then false
        else if length S = 2 andalso nth S 1 = OPAREN then false 
        else
          let 
            fun Count(x,p) = 
              case (x,p) of 
                (NONE,_) => NONE
              | (SOME c,OPAREN) => SOME (c+1)
              | (SOME c,CPAREN) => if (c=0) then NONE else SOME (c-1)
          in
            iter Count (SOME 1) (subseq S (1,length(S)-1)) = SOME 0
          end 
  自己的、
  fun parenMatch S = if length S <= 1 then false
        else if nth S 0 = CPAREN then false
        else if length S = 2 andalso nth S 1 = OPAREN then false 
        else
          let 
            val lenx = length(S) 
            fun Count(x,p) = 
              case (x,p) of 
                ((NONE,i),_) => (NONE,i+1)
              | ((SOME c,i),OPAREN) =>if c=0 andalso i<lenx-1 then (NONE,i+1) else (SOME (c+1),i+1)
              | ((SOME c,i),CPAREN) => if (c=0) then (NONE,i+1) else (SOME (c-1),i+1)
          in
            iter Count (SOME 1,1) (subseq S (1,lenx-2)) = (SOME 0,lenx-1)
          end 