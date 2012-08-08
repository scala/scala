object Test {
  val col = collection.mutable.Queue(1,2,3)
  val WORK: collection.mutable.Queue[Int] = col filterNot (_ % 2 == 0)
}
