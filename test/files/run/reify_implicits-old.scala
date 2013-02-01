import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends App {
  reify {
    implicit def arrayWrapper[A : ClassManifest](x: Array[A]) =
      new {
        def sort(p: (A, A) => Boolean) = {
          util.Sorting.stableSort(x, p); x
        }
      }
    val x = Array(2, 3, 1, 4)
    println("x = "+ x.sort((x: Int, y: Int) => x < y).toList)
  }.eval
}