import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends App {
  reify {
    def sort(a: List[Int]): List[Int] = {
      if (a.length < 2)
        a
      else {
        val pivot = a(a.length / 2)
        sort(a.filter(_ < pivot)) :::
             a.filter(_ == pivot) :::
             sort(a.filter(_ > pivot))
      }
    }

    val xs = List(6, 2, 8, 5, 1)
    println(xs)
    println(sort(xs))
  }.eval
}