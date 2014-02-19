class Bippy[T](val value: T) extends annotation.StaticAnnotation

class A {
  @Bippy("hi") def f1: Int = 1
  @Bippy[String]("hi") def f2: Int = 2

  @throws("what do I throw?") def f3 = throw new RuntimeException
  @throws[RuntimeException]("that's good to know!") def f4 = throw new RuntimeException
}

object Test {
  import scala.reflect.runtime.universe._

  def main(args: Array[String]): Unit = {
    val members = typeOf[A].decls.toList
    val tpes = members flatMap (_.annotations) map (_.tree.tpe)

    tpes.map(_.toString).sorted foreach println
  }
}
