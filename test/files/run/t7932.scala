class Category[M[_, _]]
trait M[F] {
  type X[a, b] = F
  def category: Category[X] = null
  def category1: Category[Tuple2] = null
}
abstract class C extends M[Float]
object Test extends App {
  val ms = classOf[C].getMethods.filter(_.getName.startsWith("category"))
  println(ms.map(_.toGenericString).sorted.mkString("\n"))
}
