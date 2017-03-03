import scala.collection.parallel.immutable.ParSet
 
object Test {
  def main(args: Array[String]): Unit = {
    for (i <- 1 to 2000) test()
  }
 
  def test() {
    ParSet[Int]((1 to 10000): _*) foreach (x => ()) // hangs non deterministically
  }
}

