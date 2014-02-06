object t6948 {
  val rand = new scala.util.Random()
  def a1 = rand.shuffle(0 to 5)
  // Tis not to be
  // def a2 = rand.shuffle(0 until 5)
  def a3 = rand.shuffle(Vector(1, 2, 3))
  def a4 = rand.shuffle(scala.collection.Seq(1, 2, 3))
  def a5 = rand.shuffle(scala.collection.immutable.Seq(1, 2, 3))
  def a6 = rand.shuffle(scala.collection.mutable.Seq(1, 2, 3))
}
