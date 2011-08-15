object Test extends App {
  val a = Array(1, 2, 3)
  println(a.deep.toString)

  val aaiIncomplete = new Array[Array[Array[Int]]](3)
  println(aaiIncomplete(0))

  val aaiComplete: Array[Array[Int]] = Array.ofDim[Int](3, 3) // new Array[Array[Int]](3, 3)
  println(aaiComplete.deep)
  for (i <- 0 until 3; j <- 0 until 3)
    aaiComplete(i)(j) = i + j
  println(aaiComplete.deep.toString)
  assert(aaiComplete.last.last == 4)
}
