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
  test(assert((new Refinements.Baz[String, Int, FooStringInt].baz(new FooStringInt): (String, Int)) === ("I am foo", 23)))
  test(assert((new Refinements.Qux[String, Int, FooString with FooInt].qux( new FooString with FooInt ): (String, Int)) === ("I am foo", 23)))
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
  }

  test(assert(new Refinements.MethodOrPoly1[Int, MethodicComplexInt].read(new MethodicComplexInt, "23") === 23))
  test(assert(new Refinements.MethodOrPoly2[Int, MethodicComplexInt].read(new MethodicComplexInt, "23", true) === 24))
  test(assert(new Refinements.MethodOrPoly3[Int, MethodicComplexInt].read(new MethodicComplexInt, "23", true, 6) === 30))

  class EncoderInt {
    def encode(t: Int): String = t.toString
  }

  class OutputterInt1 {
    val output: Int = 23
  }

  class OutputterInt2 {
    def output: Int = 23
  }

  test(assert(new Refinements.Structural1[OutputterInt1].get(new OutputterInt1) === 23))
  test(assert(new Refinements.Structural2[OutputterInt2].get(new OutputterInt2) === 23))
  test(assert(new Refinements.Structural3[EncoderInt].encodeWith(new EncoderInt, 23) === "23"))

  test(assert(new Refinements.StructuralFlip1().get(new OutputterInt1) === 23))
  test(assert(new Refinements.StructuralFlip2().get(new OutputterInt2) === 23))
  test(assert(new Refinements.StructuralFlip3().encodeWith(new EncoderInt, 23) === "23"))

  class EncoderIntSel extends Selectable {

    def encode(t: Int): String = t.toString

    def selectDynamic(name: String): Any = ???
    override def applyDynamic(name: String, paramClasses: Seq[ClassTag[_]])(args: Seq[Any]): Any = (name, paramClasses, args) match {
      case ("encode", Seq(ClassTag.Int), Seq(i: Int)) => encode(i)
    }

  }

  test(assert(new Refinements.StructuralSelectable[EncoderIntSel].encodeWith(new EncoderIntSel, 23) === "23"))
  test(assert(new Refinements.StructuralSelectableFlip().encodeWith(new EncoderIntSel, 23) === "23"))

}
