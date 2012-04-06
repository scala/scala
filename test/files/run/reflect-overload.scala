object Test extends App {
  import reflect.mirror._

  val s = "hello world"
  val sc = symbolOfInstance(s)
  val st = sc.asType
  val m = st member newTermName("indexOf")
  val IntType = definitions.IntClass.asType
  val indexOf = m resolveOverloaded(actuals = List(IntType))
  assert(invoke(s, indexOf)('w') == 6)
  assert((invoke(s, indexOf)('w') match { case x: Int => x }) == 6)

  val m2 = st member newTermName("substring")
  val substring = m2 resolveOverloaded(actuals = List(IntType, IntType))
  assert(invoke(s, substring)(2, 6) == "llo ")
}
