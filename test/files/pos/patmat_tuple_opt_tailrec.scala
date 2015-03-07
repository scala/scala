import scala.annotation.tailrec

object Test {
  @tailrec final def rec(a: Any, b: Any): Unit =
    (a, b) match {
      case (_, _) =>
        null match {
          case _ => rec(a, b)
        }
    }
}
