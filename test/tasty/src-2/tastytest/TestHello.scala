package tastytest

object TestHello {

  def test1  = assert(HelloWorld.msg1 === "Hello, World!")
  def test2  = assert(HelloWorld.msg2 === "Hello, World!")
  def test3  = assert((HelloWorld.msg3: "Hello, World!") === "Hello, World!")
  def test4  = assert(HelloWorld.ints.exists(_ > 2))
  def test5  = assert((HelloWorld.one: 1) === 1)
  def test6  = assert(HelloWorld.inferred(1) === 2)
  def test7  = assert(HelloWorld.the[Numeric[Int]].plus(1, 2) === 3)
  def test8  = assert(HelloWorld.bounded("a") === "aa")
  def test9  = assert(HelloWorld.higher[Option, IterableOnce](Some(1)) === (Some(1): IterableOnce[Int]))
  def test10 = assert(HelloWorld.higherBounded(List(1,2,3)) === List(1,2,3))
  def test11 = assert(HelloWorld.func(101) === "101")
  def test12 = assert(HelloWorld.func1(33) === 33)

  def main(args: Array[String]): Unit = {
    test1
    test2
    test3
    test4
    test5
    test6
    test7
    test8
    test9
    test10
    test11
    test12
    println("Suite passed!")
  }
}