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
}
