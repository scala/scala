class Base[T]

object Foo1 {
  def apply[T](x: Int): Base[T] = new Base[T]
  def apply[T](x: Int, z: String="abc"): String = z
}

object Foo2 {
  def apply[T](a: Int): Base[T] = new Base[T]
  def apply[T]: String = "abc"
}

object Foo3 {
  def apply[T](x: Int): Base[T] = new Base[T]
  def apply[T](implicit z: String): String = z
}

object Foo4 {
  def apply[T](x: Int): Base[T] = new Base[T]
  def apply[T](x: Int)(implicit z: String): Base[T] = new Base[T]
}

object Test1 {
  def test1[T]          = Foo1[T](1)
  def test2[T]: String  = Foo1[T](1)
  def test3[T]: Base[T] = Foo1[T](1)
  def test4[T]: Int     = Foo1[T](1)

}

object Test2 {
  def test5[T]          = Foo2[T]
  def test6[T]: String  = Foo2[T]
  def test7[T]: Base[T] = Foo2[T]
  def test8[T]: Int     = Foo2[T]
}

object Test3 {
  implicit val v: String = "abc"
  def test9[T]: Int     = Foo3[T]
  def test10[T]: Base[T] = Foo3[T]
  def test11[T]: String  = Foo3[T]
  def test12[T]          = Foo3[T]
}

object Test4 {
  def test13[T]: Int     = Foo3[T]
  def test14[T]: Base[T] = Foo3[T]
  def test15[T]: String = Foo3[T]
  def test16[T]         = Foo3[T]
}

object Test5 {
  def test17[T]          = Foo4[T](1)
  def test18[T]: Base[T] = Foo4[T](1)
  //def test19[T]: String  = Foo4[T](1) // #5554
}


