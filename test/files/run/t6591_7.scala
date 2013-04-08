import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends App {
  locally {
    val x = 2
    def y = 3
    var z = 4
    class C {
      var w = 5
      locally {
        val expr = reify(x + y + z + w)
        // blocked by SI-7103, though it's not the focus of this test
        // therefore I'm just commenting out the evaluation
        // println(expr.eval)
        expr.tree.freeTerms foreach (ft => {
          // blocked by SI-7104, though it's not the focus of this test
          // therefore I'm just commenting out the call to typeSignature
          // println(s"name = ${ft.name}, sig = ${ft.typeSignature}, stable = ${ft.isStable}")
          println(s"name = ${ft.name}, stable = ${ft.isStable}")
        })
      }
    }
    new C()
  }
}