package tastytest

object TestHello extends Suite("TestHello") {

  test(assert(HelloWorld.msg1 === "Hello, World!"))
  test(assert(HelloWorld.msg2 === "Hello, World!"))
  test(assert(HelloWorld.msg3 === "Hello, World!"))
  test(assert((HelloWorld.msg3: "Hello, World!") === "Hello, World!"))
  test(assert(HelloWorld.ints.exists(_ > 2)))
  test(assert((HelloWorld.one: 1) === 1))
  test(assert(HelloWorld.inferred(1) === 2))
  test(assert(HelloWorld.the[Numeric[Int]].plus(1, 2) === 3))
  test(assert(HelloWorld.bounded("a") === "aa"))
  test(assert(HelloWorld.higher[Option, IterableOnce](Some(1)) === (Some(1): IterableOnce[Int])))
  test(assert(HelloWorld.higherBounded(List(1,2,3)) === List(1,2,3)))
  test(assert(HelloWorld.higherBounded2(List(1,2,3)) === List(1,2,3)))
  test(assert(HelloWorld.higherBounded3(List(List(1))) === List(List(1))))
  test(assert(HelloWorld.higherBounded4(Left[Int,String](3)) === Left[Int,String](3)))
  test(assert(HelloWorld.higherBounded5(List(1,2,3)) === List(1,2,3)))
  test(assert(HelloWorld.higherBounded6(ShowString) === (ShowString: Show[String])))
  test(assert(HelloWorld.higherBounded7(orInt("")) === orInt("")))
  test(assert(HelloWorld.repeated("a","b","c") === "a,b,c"))
  test(assert(HelloWorld.func(101) === "101"))
  test(assert(HelloWorld.func1(33) === 33))
  test(assert((HelloWorld.lzy: "lazy") === "lazy"))
  test(assert(HelloWorld.acceptsOnlyMsg4(HelloWorld.msg4) === "Hello, World!Hello, World!"))
  test(assert(HelloWorld.`1) my long test assertion that 1^3 == 2` === 2))
  // test(assert(HelloWorld.`<init>` === 157)) // wait until https://github.com/lampepfl/dotty/issues/7799

  type OrInt[A] = Either[Int, A]

  def orInt[A](a: A): OrInt[A] = Right(a)

  trait Show[-A]

  object ShowString extends Show[String]

}
