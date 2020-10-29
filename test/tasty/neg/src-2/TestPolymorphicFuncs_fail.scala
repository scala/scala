package tastytest

object TestPolymorphicFuncs {
  def test1 = PolymorphicFuncs.id(23) // error
  def test2 = PolymorphicFuncs.takesId(new PolyFunction{def apply[T](t: T): T = t}) // error
  def test3 = {
    val box = new PolymorphicFuncs.PolyBox[PolyFunction{def apply[T](t: T): T}] // error
    box.takesId(new PolyFunction{def apply[T](t: T): T = t})
  }
}
