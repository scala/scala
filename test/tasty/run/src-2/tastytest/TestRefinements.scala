package tastytest

object TestRefinements extends Suite("TestRefinements") {

  class FooStringInt extends Refinements.Foo {
    type T = String
    type U = Int
    def foo = ("I am foo", 23)
  }

  trait FooString extends Refinements.FooT {
    type T = String
    def fooT = "I am foo"
  }

  trait FooInt extends Refinements.FooU {
    type U = Int
    def fooU = 23
  }

  class MethodicInt extends Refinements.Methodic {
    def nullary: Int = 23
    def nillary(): Int = 23
    def poly[T](): Int = 23
    val value: Int = 23
  }

  test(assert((new Refinements.Bar[String, Int].bar(new FooStringInt)) == ("I am foo", 23)))

  test(assert((new Refinements.Baz[String, Int, FooStringInt].baz(new FooStringInt): (String, Int)) === ("I am foo", 23)))

  test(assert((new Refinements.Qux[String, Int, FooString with FooInt].qux( new FooString with FooInt ): (String, Int)) === ("I am foo", 23)))

  test(assert((new Refinements.Zot[String, Int].zot( new FooString with FooInt )) == ("I am foo", 23)))

  test(assert(new Refinements.Blip[Int, MethodicInt].blip(new MethodicInt) === 23))
  // test(assert(new Refinements.Blap[Int, MethodicInt].blap(new MethodicInt) === 23)) // METHODtype
  test(assert(new Refinements.Blam[Int, MethodicInt].blam(new MethodicInt) === 23))
  // test(assert(new Refinements.Bloc[Int, MethodicInt].bloc(new MethodicInt) === 23)) // POLYtype

  test(assert(new Refinements.Clip[Int].clip(new MethodicInt) === 23))
  test(assert(new Refinements.Clap[Int].clap(new MethodicInt) === 23))
  test(assert(new Refinements.Clam[Int].clam(new MethodicInt) === 23))
  test(assert(new Refinements.Cloc[Int].cloc(new MethodicInt) === 23))
}
