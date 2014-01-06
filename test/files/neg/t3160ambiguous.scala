object Bippy {
  private class List[+T]
}
class Bippy {
  import Bippy._
  import scala.collection.immutable._

  def f(x: List[Any]): String = ???  // ambiguous, because Bippy.List is accessible
}
class Other {
  import Bippy._
  import scala.collection.immutable._

  def f(x: List[Any]): String = ???  // unambiguous, because Bippy.List is inaccessible
}
