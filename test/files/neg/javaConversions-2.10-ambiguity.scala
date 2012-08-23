import collection.{JavaConversions, mutable, concurrent}
import JavaConversions._
import java.util.concurrent.{ConcurrentHashMap => CHM}

object Bar {
  def assertType[T](t: T) = t
  val a = new CHM[String, String]() += (("", ""))
  assertType[mutable.ConcurrentMap[String, String]](a)
}
// vim: set et:
