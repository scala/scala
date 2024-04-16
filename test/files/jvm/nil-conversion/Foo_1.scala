//> using options -opt:none
class Foo_1 {
  def foo: List[Int] = List()

  def bar: List[Int] = collection.immutable.List()

  def baz: List[Int] = Foo_1.MyList()

  def boo: List[Int] = List.empty
}
object Foo_1 {
  val MyList = collection.immutable.List
}
