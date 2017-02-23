object Test extends EmbeddedControls {
  import scala.reflect.SourceContext

  abstract class Record extends Struct
  case class Rep[T:Manifest](x: T)
  def unit[T:Manifest](x: T) = Rep(x)
  implicit def numericToNumericRep[T:Numeric:Manifest](x: T) = unit(x)
  implicit def numericToNumericOps[T:Numeric:Manifest](n: T) = new NumericOpsCls(unit(n))
  implicit def repNumericToNumericOps[T:Numeric:Manifest](n: Rep[T]) = new NumericOpsCls(n)
  class NumericOpsCls[T:Numeric:Manifest](lhs: Rep[T]) {
    def +[A](rhs: A)(implicit c: A => T, pos: SourceContext): Rep[A] = error("")
    def +(rhs: Rep[T])(implicit pos: SourceContext): Rep[T] = error("")
  }
  def __new[T](args: (String, Boolean, Rep[T] => Rep[_])*): Rep[T] = error("")

  val a = unit(1.0)
  val f1 = (1.0 + a)
  val foo = new Record {
    val ok = a
    val f1 = (1.0 + a)
  }
}
