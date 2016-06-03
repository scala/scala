import scala.reflect.runtime.universe._

abstract class C {
  val xAbs: Int
  val x: Int = 2
  def yAbs: Int
  def y: Int = 2
  type TAbs <: Int
  type T = Int
}
trait T {
  val zAbs: Int
  val z: Int = 2
  def wAbs: Int
  def w: Int = 2
  type UAbs <: Int
  type U = Int
}
class AllConcrete extends C {
  val xAbs = 3
  def yAbs = 3
}
object M

object Test extends App {
  println("Testing Symbol.isAbstract...")
  def test[T: TypeTag] = {
    val sym = typeOf[T].typeSymbol
    println(s"=======$sym=======")
    def printAbstract(sym: Symbol) = println(s"$sym => ${if (sym.isAbstract) "abstract" else "concrete"}")
    printAbstract(sym)
    sym.info.decls.sorted.foreach(printAbstract)
  }
  test[C]
  test[T]
  test[AllConcrete]
  test[M.type]
}