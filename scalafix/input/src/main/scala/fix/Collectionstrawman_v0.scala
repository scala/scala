/* ONLY
rewrite = "scala:fix.Collectionstrawman_v0"
patches.replaceSymbols = [
  { from = "scala.collection.immutable.HashMap",
    to   = "strawman.collection.immutable.HashMap" }
  { from = "scala.collection.immutable.Map",
    to   = "strawman.collection.immutable.Map" }
  { from = "scala.Predef.Map",
    to   = "strawman.collection.immutable.Map" }
  { from = "scala.collection.immutable.List",
    to   = "strawman.collection.immutable.List" }
  { from = "scala.collection.immutable.Nil",
    to   = "strawman.collection.immutable.Nil" }
  { from = "scala.package.Stream",
    to   = "strawman.collection.immutable.LazyList" }
  { from = "scala.package.`#::`",
    to   = "strawman.collection.immutable.LazyList.`#::`" }
  { from = "scala.package.Vector",
    to   = "strawman.collection.immutable.Vector" }
  { from = "scala.collection.mutable.ArrayBuffer",
    to   = "strawman.collection.mutable.ArrayBuffer" }
]
*/
package fix

object Collectionstrawman_v0_List {
  List(1, 2, 3)
  1 :: 2 :: 3 :: Nil
  val isEmpty: List[_] => Boolean = {
    case Nil     => true
    case x :: xs => false
  }
}

object Collectionstrawman_v0_Stream {
  Stream(1, 2, 3)
  1 #:: 2 #:: 3 #:: Stream.Empty
  val isEmpty: Stream[_] => Boolean = {
    case Stream.Empty => true
    case x #:: xs     => false
  }
}

object Collectionstrawman_v0_Vector {
  val xs: Vector[Int] = Vector(1, 2, 3)
}

object Collectionstrawman_v0_Seq {
  val xs: Seq[Int] = Seq(1, 2, 3)
}

object Collectionstrawman_v0_Map {
  val xs: Map[Int, String] = Map(1 -> "1", 2 -> "2", 3 -> "3")
  import scala.collection.immutable.HashMap
  val ys = HashMap.empty
}

object Collectionstrawman_v0_ArrayBuffer {
  import scala.collection.mutable.ArrayBuffer
  val xs: ArrayBuffer[Int] = ArrayBuffer(1, 2, 3)
}

object Collectionstrawman_v0_ArrayAndString {
  def foo(xs: Array[Int], ys: String): Unit = {
    xs.map(x => x + 1)
    ys.map(c => c.toUpper)
  }
}

object Collectionstrawman_v0_Range {
  for (i <- 1 to 10; j <- 0 until 10) yield (i, j)
}
