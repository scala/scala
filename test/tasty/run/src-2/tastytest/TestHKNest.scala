package tastytest

import HKNest._

object TestHKNest extends Suite("TestHKNest") {

  test(assert(new HKClass_1[Foo].foo(new Foo[Box]) == "Foo"))
  test(assert(new HKClass_2[Bar].foo(new Bar[Box, String]) == "Bar"))
  test(assert(new HKClass_3[Baz].foo(new Baz[List]) == "Baz"))
  test(assert(new HKClass_4[Qux].foo(new Qux[QuxArg]) == "Qux"))
  test(assert(new HKClass_4[Qux].foo(new Qux[Arg1]) == "Qux"))
  test(assert(new HKClass_5[Quux].foo(new Quux[QuxArg]) == "Quux"))
  test(assert(new HKClass_5[Quux].foo(new Quux[Arg1]) == "Quux"))
  test(assert(new HKClass_6[Quux].foo(new Quux[QuxArg]) == "Quux"))
  test(assert(new HKClass_7[Boo].foo(new Boo[OrInt]) == "Boo"))
  test(assert(new HKClass_8[ThrowawayHK].foo[List](new Foo[({ type Lambda[T] = Either[Nothing, T]})#Lambda]) == "Foo"))
  test(assert(new HKClass_9[ThrowawayHK].foo[List](new Foo[({ type Lambda[T] = Either[Nothing, T]})#Lambda]) == "Foo"))

  type ListInt[I]
  // test(assert(new HKClass_8[Boo].foo(new Boo[StringOrInt]) == "Boo"))  blocked by https://github.com/lampepfl/dotty/issues/8329

}
