object Test {
  import scala.collection.immutable

  def checkIdentity[A](xs: immutable.IndexedSeq[A]) = assert(xs.toIndexedSeq eq xs)

  def main(args: Array[String]): Unit = {
    def r = 1 to 10
    checkIdentity(immutable.Vector(r: _*))
    checkIdentity(r.toIndexedSeq)
  }
}
