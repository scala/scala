class B extends A { protected val x = false }
trait A { self: B => x }
