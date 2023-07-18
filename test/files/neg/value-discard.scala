// scalac: -Wvalue-discard -Werror
final class UnusedTest {
  import scala.collection.mutable

  def remove(): Unit = {
    mutable.Set[String]().remove("")   // warn because suspicious receiver
  }

  def removeAscribed(): Unit = {
    mutable.Set[String]().remove(""): Unit    // nowarn
  }

  def subtract(): Unit = mutable.Set.empty[String].subtractOne("")     // warn because suspicious receiver

  def warnings(): Unit = {
    val s: mutable.Set[String] = mutable.Set.empty[String]
    ""                         // warn pure expr
    "": Unit                   // nowarn
    s.subtractOne("")          // nowarn
  }

  def f(implicit x: Int): Boolean = x % 2 == 1

  implicit def i: Int = 42

  def u: Unit = f: Unit       // nowarn

  def body = 27
  def `single case is one-legged if`(i: Int): Unit = i match {
    case 42 => body           // nowarn
    case _ =>
  }
  def `empty case is required`(i: Int): Unit = i match {
    case 42 => body           // warn
  }
}
