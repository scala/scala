// exercise coevolveSym
trait T[A] { val foo: { type B = A } = ???; def bar(b: foo.B) = () }
object O extends T[Int] { bar(0) }
