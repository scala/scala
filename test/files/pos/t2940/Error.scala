abstract class Error {
	val c: Cycle[_]
}

object Test {
  trait Quux[T] extends Cycle[Quux[T]]
  val x = new Quux[Int] { def doStuff() { } }

  def main(args: Array[String]): Unit = {

  }
}
