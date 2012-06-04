import scala.reflect.{basis => rb}
import scala.reflect.runtime.{universe => ru}

object Test {
  def main(args: Array[String]) {
    def foo(implicit t: rb.TypeTag[List[Int]]) {
      println(t)
      val t2: ru.TypeTag[_] = t in ru.rootMirror
      println(t2)
    }
  }
}
