package tastytest

import FancyColours._
import reflect.classTag

object TestFancyColours extends Suite("TestFancyColours") {

  def describe(c: Colour) = c match {
    case Colour.Pink => "Amazing!"
    case Colour.Red => "Yawn..."
  }

  def describePretty(c: Pretty) = c match {
    case Colour.Pink => "Amazing!"
  }

  def describeDull(c: Dull) = c match {
    case Colour.Red => "Yawn..."
  }

  def describeAny(x: Any) = x match {
    case p: Colour.Pink.type => Colour.describePink(p)
    case r: Colour.Red.type  => Colour.describeRed(r)
  }

  test(assert(describe(Colour.Pink) === "Amazing!"))
  test(assert(describe(Colour.Red) === "Yawn..."))

  test("casting erasure [Pretty]") {
    assert(describePretty(Colour.Pink) === "Amazing!")
  }
  test("casting erasure [Dull]") {
    assert(describeDull(Colour.Red) === "Yawn...")
  }

  test("Singleton type erasure") {
    assert(Colour.describeRed(Colour.Red) === "Red")
  }

  test("Array erasure tags") {
    assert(classTag[Array[Colour.Red.type]].runtimeClass eq classTag[Array[Colour]].runtimeClass)
  }

  test("enum erasure tags") {
    assert(classTag[Colour.Red.type].runtimeClass eq classTag[Colour].runtimeClass)
  }

  test("Array erasure storage") {
    val reds = new Array[Colour.Red.type](1)
    reds(0) = Colour.Red
    assert(reds(0) === Colour.Red)
  }

  test("pat mat erasure [Pink.type]") {
    assert(describeAny(Colour.Pink) === "Pink")
  }

  test("pat mat erasure [Red.type]") {
    assert(describeAny(Colour.Red) === "Red")
  }

  test(assert((Colour.Red: Any).isInstanceOf[scala.deriving.Mirror.Singleton]))

}
