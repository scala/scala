import scala.language.implicitConversions

trait Q
trait K

object Use {
  implicit def cd[T](p: T)(implicit ev: T => K): Q = ???
  val x: Q = ex.Mac.mac("asdf")
}

