object Test {
  import scala.collection.{ mutable, immutable, generic }
  
  def checkIdentity[A](xs: immutable.IndexedSeq[A]) = assert(xs.toIndexedSeq eq xs)
  
  def main(args: Array[String]): Unit = {
    checkIdentity(immutable.Vector(1 to 10: _*))
    checkIdentity(1 to 10 toIndexedSeq)
  }
}
