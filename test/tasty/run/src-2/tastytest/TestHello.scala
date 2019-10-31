package tastytest

object TestHello extends Suite("TestHello") {

  test(assert(HelloWorld.msg1 === "Hello, World!"))
  test(assert(HelloWorld.msg2 === "Hello, World!"))
  test(assert((HelloWorld.msg3: "Hello, World!") === "Hello, World!"))
  test(assert(HelloWorld.ints.exists(_ > 2)))
  test(assert((HelloWorld.one: 1) === 1))
  test(assert(HelloWorld.inferred(1) === 2))
  test(assert(HelloWorld.the[Numeric[Int]].plus(1, 2) === 3))
  test(assert(HelloWorld.bounded("a") === "aa"))
  test(assert(HelloWorld.higher[Option, IterableOnce](Some(1)) === (Some(1): IterableOnce[Int])))
  test(assert(HelloWorld.higherBounded(List(1,2,3)) === List(1,2,3)))
  test(assert(HelloWorld.func(101) === "101"))
  test(assert(HelloWorld.func1(33) === 33))
  test(assert((HelloWorld.lzy: "lazy") === "lazy"))

}
