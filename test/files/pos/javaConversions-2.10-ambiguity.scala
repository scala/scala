import collection.{mutable, concurrent}
import collection.convert.ImplicitConversionsToScala._
import java.util.concurrent.{ConcurrentHashMap => CHM}

object Bar {
  def assertType[T](t: T) = t
  val a = new CHM[String, String]() += (("", ""))
  assertType[concurrent.Map[String, String]](a)
}
// vim: set et:
