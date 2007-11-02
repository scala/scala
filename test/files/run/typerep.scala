import runtime.TypeRep._

object Test extends Application {
  testPrimitives
  testOptions
  testLists
  testArrays
  testTuples
  testFuncs
  //test4 // todo
}

object testPrimitives {
  println(getType(true))
  println(getType(16.toByte))
  println(getType('a'))
  println(getType(3))
  println(getType(3l))
  println(getType(0.0f))
  println(getType(0.0d))
  println(getType("abc"))
  println(getType(())) // Unit
  println(getType(classOf[Int])) // Class
  println
}

object testOptions {
  println(getType(Some(2)))
  //println(getType(Some(Some(3)))) // ambiguous implicit values
  //println(getType(None)) // no implicit argument matching parameter
  println
}

object testLists {
  println(getType(List(3)))
  println(getType(3 :: Nil))
  println(getType(List(List(3))))
  //println(getType(Nil))  // no implicit argument matching parameter type
  println(getType(List(1, "abc")))
  println
}

object testArrays {
  println(getType(Array(3)))
  println(getType(Array(Array(3), Array(4))))
  println(getType(new Array[Int](0)))
  println(getType(List(1).toArray))
  println(getType(List[Int]().toArray))
  println(getType(Array(3).drop(1).toArray)) // empty
  println
}

object testTuples {
  println(getType((3, "abc")))
  println(getType(Triple('a', 'b', "c")))
  println(getType(((3, "abc"), (4, "xyz"))))
  println
}

object testFuncs {
  def f1(x: Int): Int = 2 * x
  println(getType(f1 _))
  println(getType(f1(2)))
  val f2 = (x: Int) => 2 * x
  println(getType(f2))
  println(getType(f2(2)))
  val f3 = (x: Int) => (y: Int) => x + y
  println(getType(f3))
  println(getType(f3(2)))
  println(getType(f3(2)(2)))
  def f4(b: Boolean, c: List[Char], i: Int): Int = i
  println(getType(f4 _))
  def f5(f: Int => Int, x: Int) = f(x)
  println(getType(f5 _))
  println(getType(f5(f1, 1)))
  println
}

class Foo {
  class Bar(x: Int)
}

object foo extends Foo

package pkg1 {
  class C1
  object c1 extends C1
}
/*
object test4 {
  println(getType(foo))
  println(getType(new foo.Bar(0)))
  val foo2 = new Foo
  println(getType(foo2))
  println(getType(new foo2.Bar(1)))
  println

  println(getType(pkg1.c1))
  val c1 = new pkg1.C1
  println(getType(c1))
  println
}
*/
