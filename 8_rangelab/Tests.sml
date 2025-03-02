structure Tests =
struct
  structure Seq = ArraySequence
  open Seq

  type point = int * int

  (* Here are a couple of test structures. ordSet1 is a test ordered set
   * (table), while points1 is a test collection of points in the plane.
   *
   * Note that for an ordered table test, you should just specify a sequence
   * of keys (and values will be automatically set to unit). *)

  val ordSet1 = % [5, 7, 2, 8, 9, 1]

  val testsFirst = [
    ordSet1,
    % [~1,2,4],
    % [1,2,2,6],
    % []
  ]
  val testsLast = [
    ordSet1,
    % [~1,2,4],
    % [1,2,2,6],
    % []
  ]
  val testsPrev = [
    (ordSet1, 8),
    (ordSet1, 1),
    (% [~2,~1,2,4],~1),
    (% [1,2,2,6],6),
    (% [], 8)
  ]
  val testsNext = [
    (ordSet1, 8),
    (ordSet1, 9),
    (% [~2,~1,2,4],~1),
    (% [1,2,2,6],6),
    (% [], 8)
  ]
  val testsJoin = [
    (ordSet1, % [100]),
    (ordSet1, % [3]),
    (% [], % [100])
  ]
  val testsSplit = [
    (ordSet1, 7),
    (ordSet1, 100),
    (% [], 7)
  ]
  val testsRange = [
    (ordSet1, (5,8)),
    (% [~2,~1,2,4],(~1,6)),
    (% [1,2,2,6],(1,6)),
    (ordSet1, (10,12)),
    (% [], (5,8))
  ]


  val points1 = % [(0,0),(1,2),(3,3),(4,4),(5,1)]
  val points2 : point seq = % []
  val points3 = % [(10000,10000),(0,0)]
  val points4 = tabulate (fn i => (i,i)) 1000
  val ponits5 = %[(~2,~2),(~1,~1),(0,0),(1,1),(2,2)]
  val testsCount = [
    (points1, ((1,3),(5,1))),
    (points1, ((2,4),(4,2))),
    (points1, ((100,101),(101,100))),

    (points2, ((0,10),(10,0))),
    (points3, ((0,10000),(10000,0))),
    (points4, ((0,500),(1000,00))),
    (ponits5, ((~1,~1),(1,1))),

    
    (points1, ((2,4),(5,3))), 
    (points1, ((~1,3),(3,7))),
    (points1, ((6,10),(5,1))),
    (points2, ((0,10),(10,0))),
    (points3, ((6,6),(6,6))),
    (points4, ((0,500),(1000,0))) 
  ]


end
