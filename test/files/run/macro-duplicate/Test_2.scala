import scala.concurrent._
import ExecutionContext.Implicits.global

object Test extends App {
  Macros.foo
}