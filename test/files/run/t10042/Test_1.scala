object Test extends App {
  import test._

  CheckMacro.check[Subject_0]
  CheckMacro.check[Subject_1]

  import reflect.runtime.universe, universe._
  val checks = new Checks[universe.type](universe)
  checks.check(symbolOf[Subject_0].asClass)
  checks.check(symbolOf[Subject_1].asClass)
}