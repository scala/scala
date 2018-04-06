import collection.{mutable, concurrent}
import collection.JavaConverters._
import java.util.concurrent.{ConcurrentHashMap => CHM}

object Bar {
  def assertType[T](t: T) = t
  val a = new CHM[String, String]().asScala += (("", ""))
  assertType[concurrent.Map[String, String]](a)
}
