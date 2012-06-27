import scala.reflect.ClassTag

trait Trees {
  class Tree
  implicit val ttTag: ClassTag[TypeTree]
  type TypeTree <: Tree
  val TypeTree: TypeTreeExtractor
  abstract class TypeTreeExtractor {
    def unapply(t: TypeTree): Option[String]
  }
  def test(tree: Tree) =
    tree match {
      case TypeTree(_) => println("lolwut")
      case null => println("correct")
    }
}

object Test extends App with Trees {
  val ttTag = implicitly[ClassTag[TypeTree]]
  case class TypeTree(meh: String) extends Tree
  object TypeTree extends TypeTreeExtractor
  test(null) // should not crash
}