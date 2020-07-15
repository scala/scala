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
  test(assert(new HKClass_7[Quuux].foo(new Quuux[Arg1]) == "Quuux"))
  test(assert(new HKClass_7[Quuux].foo(new Quuux[QuxArg]) == "Quuux"))
  test(assert(new HKClass_8[Quuux].foo(new Quuux[QuxArg]) == "Quuux"))
  test(assert(new HKClass_9[Boo].foo(new Boo[OrInt]) == "Boo"))
  test(assert(new HKClass_10[ThrowawayHK].foo[List](new Foo[OrNothing]) == "Foo"))
  test(assert(new HKClass_11[ThrowawayHK].foo[List](new Foo[OrNothing]) == "Foo"))
  test(assert(new HKClass_12[Contra].foo(new Contra[Sum]) == "Contra"))
  test(assert(new HKClass_13[Contra].foo(new Contra[Sum]) == "Contra"))
  test(assert(new HKClass_13[Contra].foo(new Contra[Prod]) == "Contra"))


  type OrNothing[I] = Either[Nothing, I]

  // test(assert(new HKClass_8[Boo].foo(new Boo[StringOrInt]) == "Boo"))  blocked by https://github.com/lampepfl/dotty/issues/8329

}
