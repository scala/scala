trait Builder[C[_], T] {
  def +=(x: T)
  def finalise: C[T]
}

trait Buildable[C[_]] {
  def builder[T]: Builder[C,T]
}

object Test {

  implicit object buildableList extends Buildable[List] {
    def builder[T] = new Builder[List,T] {
      val buf = new scala.collection.mutable.ListBuffer[T]
      def +=(x: T) = buf += x
      def finalise = buf.toList
    }
  }

  def foo[C[_],T](x: T)(implicit b: Buildable[C]): C[T] = {
    val builder = b.builder[T]
    builder += x
    builder.finalise
  }

  val l: List[Int] = foo(8)
}