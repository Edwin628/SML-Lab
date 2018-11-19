functor MkBruteForcePD (structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open P
  open Seq
  open Option210




  fun parenDist (parens : paren seq) : int option =
      let 
       fun parenMatch S = if length S <= 1 then false
       (*parenMatch is used to decide whether the sequence is matched ,the match is 
          slightly different from the match in the textbook*)
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
            iter Count (SOME 1,1) (subseq S (1,lenx-1)) = (SOME 0,lenx)
          end 
        val len = length parens
        fun generate i = 
        (*generate i is used to find all the subsequence starting from position i*)
          let
            val j = i
            fun generatex (i,j) = 
            (*generatex is used to generate the sequence between postion i and position j
            the j is the index of the ending position, not the length*)
              if j < len then append(singleton(subseq parens (i,j-i+1)),generatex(i,j+1))
              else empty()
          in
            generatex (i,j)
          end
        val allSubseq=flatten (tabulate generate len)
        (*tabulate is used to cover all the starting position*)
        val matchedSubseq = filter parenMatch allSubseq 
        (*machedSubseq is used to find all the subsequence that meets the requirement
        then the length of them are the Parenthesis Distance*)
        fun maxDist (max,s) = if max > length(s) then max else length(s)
        val result = iter maxDist 0 matchedSubseq
      in
        if length(parens) = 0 then NONE
        else SOME result
      end
end
