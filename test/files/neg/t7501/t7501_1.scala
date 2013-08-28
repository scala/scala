object Test2 {
 def test[X](name: String) = 12
}
class strangeTest(x: Int) extends scala.annotation.StaticAnnotation

trait A {
  // When picking the type of `test`, the value parameter
  // `x` was pickled with the owner `trait A`. On unpickling,
  // it was taken to be a member!
  @strangeTest(Test2.test("test"))
  def test(x: String): Unit
}
