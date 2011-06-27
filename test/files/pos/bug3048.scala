class B
object C extends B

class F[T <: B](cons: => T)
class F2[T <: B](cons: => T) extends F(cons)

object D extends F2(C) // works
object E extends F2(new B {})
