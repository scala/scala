
class U {
  trait Transformer {
    def transform(a: Tree): Tree = ???
  }
  trait Tree
}

object Test {
  def m(u: U) = {
    class C extends u.Transformer {
      override def transform(t: u.Tree): u.Tree = {
        null match {
          case _ =>
            // crashes in GenICode:
            // error: Unknown type: <notype>, <notype> [class scala.reflect.internal.Types$NoType$, class scala.reflect.internal.Types$NoType$] TypeRef? false
            (y: Any) => super.transform(???)
            null
        }
        ???
      }
    }
  }
}
