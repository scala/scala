import scala.reflect.runtime.universe._, definitions._
object Test extends App {
  // Tuples
  assert(TupleClass.seq.size == 22)
  assert(TupleClass(0) == NoSymbol)
  assert(TupleClass(23) == NoSymbol)
  assert((1 to 22).forall { i => TupleClass(i).name.toString == s"Tuple$i" })
  // Functions
  assert(FunctionClass.seq.size == 23)
  assert(FunctionClass(-1) == NoSymbol)
  assert(FunctionClass(23) == NoSymbol)
  assert((0 to 22).forall { i => FunctionClass(i).name.toString == s"Function$i" })
  // Products
  assert(ProductClass.seq.size == 23)
  assert(ProductClass(-1) == NoSymbol)
  assert(ProductClass(0) == UnitClass)
  assert(ProductClass(23) == NoSymbol)
  assert((1 to 22).forall { i => ProductClass(i).name.toString == s"Product$i" })
}
