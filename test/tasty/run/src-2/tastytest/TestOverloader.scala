package tastytest

object TestOverloader extends Suite("TestOverloader") {

  test("""overloader.foo("abc", true) == "abctrue"""")(assert(new Overloader().foo("abc", true) === "abctrue"))
  test("""overloader.foo("abc") == "abcabc"""")(assert(new Overloader().foo("abc") === "abcabc"))
  test("overloader.foo == 13")(assert(new Overloader().foo === 13))

}
