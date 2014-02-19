import scala.reflect.runtime.universe._

object Test {
  trait Foo { def f: Int = 5 ; def g: Int }
  trait Bar extends Foo { def f: Int ; def g: Int = 5 }

  def main(args: Array[String]): Unit = {
    // We should see g, but not f or $init$.
    typeOf[Bar].decls.toList.flatMap(_.overrides) foreach println
  }
}
