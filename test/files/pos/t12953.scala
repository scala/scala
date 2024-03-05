
//> using options -Werror -Wunused:privates

package example

import scala.reflect.macros.blackbox

/*case*/ class A(x: Int)

class MyMacro(val c: blackbox.Context) {
  import c.universe._

  def impl(tree: c.Tree): Tree = {
    tree match {
      case q"${a: A}" =>
        //reify(a).tree // uses $m in expansion
        reify(()).tree
      case _ =>
        c.abort(c.enclosingPosition, "err")
    }
  }

  private implicit def instance: Unliftable[A] = ???
}
