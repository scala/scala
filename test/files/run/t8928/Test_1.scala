//> using javaOpt -Dneeds.forked.jvm
import test._

object Test extends App {
  Macros.apply[Annotated_0]
  Macros.apply[Annotated_1]

  import reflect.runtime.universe
  import universe._

  /* consistency check with runtime universe */
  val checks = new Checks[universe.type](universe, ordered = false)
  checks.check(weakTypeOf[Annotated_0])
  checks.check(weakTypeOf[Annotated_1])


}