class Base[T]

object Foo1 {
  def apply[T](x: Int): Base[T] = new Base[T]
  def apply[T](x: Int)(implicit z: String): String = z
}

object Foo2 {
  def apply[T](x: Int): Base[T] = new Base[T]
  def apply[T](x: Int)(implicit z: String): Base[T] = new Base[T]
}

object Test1 {
  def test1[T]: Int = Foo1[T](1)
  def test2[T]: Base[T] = Foo1[T](1)
  def test3[T]: String = Foo1[T](1)
  def test4[T] = Foo1[T](1)
}

object Test2 {
  implicit val v: String = "foo"
  def test5[T]: Int = Foo1[T](1)
  def test6[T]: Base[T] = Foo1[T](1)
  def test7[T]: String = Foo1[T](1)
  def test8[T] = Foo1[T](1)
}

object Test3 {
  def test9[T]: String = Foo2[T](1)
  def test10[T]: Base[T] = Foo2[T](1)
  def test11[T] = Foo2[T](1)
}

object Test4 {
  implicit val v: String = "foo"
  def test12[T]: String = Foo2[T](1)
  def test13[T]: Base[T] = Foo2[T](1)
  def test14[T] = Foo2[T](1)
}
