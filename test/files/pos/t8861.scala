
trait Test {
  type R = PartialFunction[Any, Unit]

  val x: R = { case "" => }
  val y: R = { case "" => }

  val z: R = x orElse y
  val zz   = x orElse y
}

