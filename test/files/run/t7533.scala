import scala.reflect.runtime.universe._

abstract class C {
  val x1: Int
  val x2: Int = 2
  def y1: Int
  def y2: Int = 2
  type T1 <: Int
  type T2 = Int
}
trait T {
  val z1: Int
  val z2: Int = 2
  def w1: Int
  def w2: Int = 2
  type U1 <: Int
  type U2 = Int
}
class D extends C {
  val x1 = 3
  def y1 = 3
}
object M

object Test extends App {
  println("Testing Symbol.isAbstract...")
  def test[T: TypeTag] = {
    val sym = typeOf[T].typeSymbol
    println(s"=======$sym=======")
    def printAbstract(sym: Symbol) = println(s"$sym => ${sym.isAbstract}")
    printAbstract(sym)
    sym.info.decls.sorted.foreach(printAbstract)
  }
  test[C]
  test[T]
  test[D]
  test[M.type]
}