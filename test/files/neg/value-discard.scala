//> using options -Wvalue-discard -Werror
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
}

class UnitAscription {
  import scala.concurrent._, ExecutionContext.Implicits._

  case class C(c: Int) {
    def f(i: Int, j: Int = c) = i + j
  }

  def f(i: Int, j: Int = 27) = i + j

  def g[A]: List[A] = Nil

  def i: Int = 42

  def `default arg is inline`: Unit =
    f(i = 42): Unit // nowarn

  def `default arg requires block`: Unit =
    C(27).f(i = 42): Unit // nowarn

  def `application requires implicit arg`: Unit =
    Future(42): Unit // nowarn

  def `application requires inferred type arg`: Unit =
    g: Unit // nowarn

  def `implicit selection from this`: Unit =
    i: Unit // nowarn
}

object UnitAscription {
  def g[A]: List[A] = Nil
  def `application requires inferred type arg`: Unit =
    g: Unit // nowarn UnitAscription.g
}
