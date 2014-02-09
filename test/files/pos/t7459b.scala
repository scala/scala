import scala.concurrent._
import scala.util._


class Test {
  (null: Any) match {
    case s @ Some(_) => ???
    case f @ _ => 
      () => f
      ???
  }
}