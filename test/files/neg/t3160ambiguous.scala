object Bippy {
  private class Node
}
class Bippy {
  import Bippy._
  import scala.xml._

  def f(x: Node): String = ???  // ambiguous, because Bippy.Node is accessible
}
class Other {
  import Bippy._
  import scala.xml._

  def f(x: Node): String = ???  // unambiguous, because Bippy.Node is inaccessible
}
