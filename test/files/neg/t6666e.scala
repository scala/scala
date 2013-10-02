
import scala.collection.immutable.TreeMap
import scala.math.Ordering


class Crash(a: Any) {
  def this() =
    this(Nil.collect{case x =>})
}
