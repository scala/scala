// scalac: -Ywarn-value-discard -Xfatal-warnings
final class UnusedTest {
  import scala.collection.mutable

  def remove(): Unit = {
    mutable.Set[String]().remove("")   // expected to warn
  }

  def removeAscribed(): Unit = {
    mutable.Set[String]().remove(""): Unit    // no warn
  }

  def subtract(): Unit = mutable.Set.empty[String].subtractOne("")     // warn

  def warnings(): Unit = {
    val s: mutable.Set[String] = mutable.Set.empty[String]
    ""                         // warn
    "": Unit                   // no warn
    s.subtractOne("")          // no warn
  }
}
