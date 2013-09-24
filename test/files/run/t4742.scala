trait T { val x: Int = 0 }
object O extends T { override final val x = 1 }

object Test extends App {
  // was throwing an UnitializedFieldError as constant 1 is folded into the accessor
  assert((O: T).x == 1)
}
