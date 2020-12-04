package tastytest

import scala.reflect.ClassTag

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

  test(assert((new Refinements.Bar[String, Int].bar(new FooStringInt)) == ("I am foo", 23)))
  test(assert((new Refinements.Baz[String, Int, FooStringInt].baz(new FooStringInt)) == ("I am foo", 23)))
  test(assert((new Refinements.Qux[String, Int, FooString with FooInt].qux( new FooString with FooInt )) == ("I am foo", 23)))
  test(assert((new Refinements.Zot[String, Int].zot( new FooString with FooInt )) == ("I am foo", 23)))

  class MethodicInt extends Refinements.Methodic {
    def nullary: Int = 23
    def nillary(): Int = 23
    def poly[T]: Int = 23
    def polyNillary[T](): Int = 23
    val value: Int = 23
  }

  test(assert(new Refinements.Blip[Int, MethodicInt].blip(new MethodicInt) === 23))
  test(assert(new Refinements.Blap[Int, MethodicInt].blap(new MethodicInt) === 23))
  test(assert(new Refinements.Blam[Int, MethodicInt].blam(new MethodicInt) === 23))
  test(assert(new Refinements.Bloc[Int, MethodicInt].bloc(new MethodicInt) === 23))
  test(assert(new Refinements.Blaa[Int, MethodicInt].blaa(new MethodicInt) === 23))

  test(assert(new Refinements.Clip[Int].clip(new MethodicInt) === 23))
  test(assert(new Refinements.Clap[Int].clap(new MethodicInt) === 23))
  test(assert(new Refinements.Clam[Int].clam(new MethodicInt) === 23))
  test(assert(new Refinements.Cloc[Int].cloc(new MethodicInt) === 23))
  test(assert(new Refinements.Claa[Int].claa(new MethodicInt) === 23))

  class MethodicComplexInt extends Refinements.MethodicComplex {
    def one(a: String): Int = a.toInt
    def two(a: String)(b: Boolean): Int = a.toInt + (if (b) 1 else 0)
    def three[C](a: String)(b: Boolean)(c: C): Int = a.toInt + (if (b) 1 else 0) + c.toString.toInt
    def implicitly(implicit ls: List[String]): Int = ls.size
    def contextual(implicit ls: List[String]): Int = ls.size
  }

  def testWithList(op: List[String] => Unit) = test(op(List.fill(3) {""}))

  test(assert(new Refinements.MethodOrPoly1[Int, MethodicComplexInt].read(new MethodicComplexInt, "23") === 23))
  test(assert(new Refinements.MethodOrPoly2[Int, MethodicComplexInt].read(new MethodicComplexInt, "23", true) === 24))
  test(assert(new Refinements.MethodOrPoly3[Int, MethodicComplexInt].read(new MethodicComplexInt, "23", true, 6) === 30))
  testWithList { implicit ls =>
    assert(new Refinements.MethodOrPoly4[Int, MethodicComplexInt].read(new MethodicComplexInt) === 3)
  }
  testWithList { implicit ls =>
    assert(new Refinements.MethodOrPoly5[Int, MethodicComplexInt].read(new MethodicComplexInt) === 3)
  }
  testWithList { implicit ls =>
    assert(new Refinements.MethodOrPoly4_2[Int].read[MethodicComplexInt](new MethodicComplexInt) === 3)
  }
  testWithList { implicit ls =>
    assert(new Refinements.MethodOrPoly5_2[Int].read[MethodicComplexInt](new MethodicComplexInt) === 3)
  }

  test {
    import Refinements._
    class BoxHello extends Box {
      val value: "Hello" = "Hello"
    }
    val preciseRefinement = new PreciseRefinement[Box { val value: "Hello" }]
    assert((preciseRefinement.identity(new BoxHello).value: "Hello") === "Hello")
    assert((preciseRefinement.valueOf(new BoxHello): "Hello") === "Hello")
  }

  class EncoderInt {
    def encode(t: Int): String = t.toString
  }

  class OutputterInt1 {
    val output: Int = 23
  }

  class OutputterInt2 {
    def output: Int = 23
  }

  class TString {
    type T = String
    val t: T = "I am TString"
  }

  test(assert(new Refinements.Structural1[OutputterInt1].get(new OutputterInt1) === 23))
  test(assert(new Refinements.Structural2[OutputterInt2].get(new OutputterInt2) === 23))
  test(assert(new Refinements.Structural3[EncoderInt].encodeWith(new EncoderInt, 23) === "23"))

  test(assert(new Refinements.StructuralFlip1().get(new OutputterInt1) === 23))
  test(assert(new Refinements.StructuralFlip2().get(new OutputterInt2) === 23))
  test(assert(new Refinements.StructuralFlip3().encodeWith(new EncoderInt, 23) === "23"))
  test(assert(new Refinements.StructuralFlip4().get(new OutputterInt1) === 23))
  test(assert(new Refinements.StructuralFlip5().get(new OutputterInt2) === 23))
  test(assert(new Refinements.StructuralFlip6().encodeWith(new EncoderInt, 23) === "23"))
  test(assert(new Refinements.StructuralTypeAliasFlip().get(new TString) === "I am TString"))
  test(assert(new Refinements.StructuralTypeBoundsFlip().get(new TString) === "I am TString"))

  class StringSpecialRefinement extends Refinements.SpecialRefinement {
    def pickOne(as: String*): Option[String] = as.headOption
    def eval(as: => String): String          = as
  }

  test(assert(new Refinements.EvalSpecialRefinement_1[StringSpecialRefinement].run(new StringSpecialRefinement, "hello") === "hello"))
  test(assert(new Refinements.EvalSpecialRefinement_2().run(new StringSpecialRefinement, "hello") === "hello"))
  test(assert(new Refinements.PickOneRefinement_1[StringSpecialRefinement].run(new StringSpecialRefinement, "hello", "world") === Option("hello")))
  test(assert(new Refinements.PickOneRefinement_2().run(new StringSpecialRefinement, "hello") === Option("hello")))

  type EncoderIntSel = Refinements.MethodSelectable {
    def encode(t: Int): String
  }
  object EncoderIntSel {
    import Refinements.MethodSelectable._

    def create: EncoderIntSel = {
      new Refinements.MethodSelectable(
        method("encode", classOf[Int]) { xs => xs.head.toString }
      ).asInstanceOf[EncoderIntSel]
    }
  }

  test(assert(new Refinements.StructuralSelectable[EncoderIntSel].encodeWith(EncoderIntSel.create, 23) === "23"))
  test(assert(new Refinements.StructuralSelectableFlip().encodeWith(EncoderIntSel.create, 23) === "23"))

}
