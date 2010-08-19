object Test {
  type Point = Map[Symbol, String]
  type Points = IndexedSeq[Point]

  def makePoints2: Points = IndexedSeq[Point]()
  val spoints2 = util.Random.shuffle(makePoints2)
}
