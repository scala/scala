trait TreesBase {
  type Tree

  type Apply <: Tree

  val Apply: ApplyExtractor

  abstract class ApplyExtractor {
    def apply(x: Int): Apply
    def unapply(apply: Apply): Option[Int]
  }
}

trait TreesApi extends TreesBase {
  def Apply(x: String)
}

class Universe extends TreesApi {
  abstract class Tree
  case class Apply(x: Int) extends Tree
  object Apply extends ApplyExtractor
  def Apply(x: String) = Apply(x.toInt)
}

object Test extends App {
  def foo(tapi: TreesApi) {
    import tapi._
    def bar(tree: Tree) {
      val Apply(x) = tree
    }
  }
}