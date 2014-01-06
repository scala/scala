object Test extends App {
  import scala.reflect.runtime.universe._
  class C
  val q"${c: C}" = q"()"
}