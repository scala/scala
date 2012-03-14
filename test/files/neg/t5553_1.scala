class Base[T]

object Foo1 {
  def apply[T](a: Int): Base[T] = new Base[T]
  def apply[T](z: String): Base[T] = new Base[T]
}

object Foo2 {
  def apply[T](a: Int): Base[T] = new Base[T]
  def apply[T](z: String="abc"): Base[T] = new Base[T]
}

object Foo3 {
  def apply[T](a: Int): Base[T] = new Base[T]
  def apply[T](z: String="abc"): String = z
}
object Test {
  def test1[T] = Foo1[T]
  def test2[T]: Int = Foo1[T]
  def test3[T]: Base[T] = Foo1[T]
}

object Test2 {
  def test4[T] = Foo2[T]
  def test5[T]: Int = Foo2[T]
  def test6[T]: Base[T] = Foo2[T]
}

object Test3{
  def test7[T] = Foo3[T]
  def test8[T]: String = Foo3[T]
  def test9[T]: Int = Foo3[T]
  def test10[T]: Base[T] = Foo3[T]
}
