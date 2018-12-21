// scalac: -Ywarn-value-discard -Xfatal-warnings
final class UnusedTest {
  import scala.collection.mutable

  def remove(): Unit = {
    mutable.Set[String]().remove("")   // expected to warn
  }

  def removeAscribed(): Unit = {
    mutable.Set[String]().remove(""): Unit
  }
}
