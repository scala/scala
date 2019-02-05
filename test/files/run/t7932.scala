import scala.language.higherKinds

class Category[M[_, _]]

trait M1[F] {
  type X[a, b] = F
  def category: Category[X] = null
  def category1: Category[Tuple2] = null
}

// The second trait is needed to make sure there's a forwarder generated in C.
// otherwise the trait methods are just the inherited default methods from M1.
trait M2[F] { self: M1[F] =>
  override def category: Category[X] = null
  override def category1: Category[Tuple2] = null
}

abstract class C extends M1[Float] with M2[Float]

object Test {
  def t(c: Class[_]) = {
    val ms = c.getMethods.filter(_.getName.startsWith("category"))
    println(ms.map(_.toGenericString).sorted.mkString("\n"))
  }
  def main(args: Array[String]): Unit = {
    t(classOf[C])
    t(classOf[M1[_]])
    t(classOf[M2[_]])
  }
}
