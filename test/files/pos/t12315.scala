// scalac: -Xfatal-warnings -Xlint
import annotation._

class Thingy[T] extends Iterator[T] with java.util.Iterator[T] {
  @nowarn
  def hasNext: Boolean = ???
  def next(): T = ???
}
