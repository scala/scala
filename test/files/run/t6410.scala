


object Test extends App {
  val x = collection.parallel.mutable.ParArray.range(1,10) groupBy { _ % 2 } mapValues { _.size }
  println(x)
  val y = collection.parallel.immutable.ParVector.range(1,10) groupBy { _ % 2 } mapValues { _.size }
  println(y)
}