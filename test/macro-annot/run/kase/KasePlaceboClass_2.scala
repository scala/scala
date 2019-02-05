package kaseannot.placebo.klass

import pkg._

@kase class CPreToplevelNocomp[T](x: T, y: Int = 2)(z: Boolean, w: String = "")
@placebo object CPreToplevelPrecomp
@kase class CPreToplevelPrecomp[T](x: T, y: Int = 2)(z: Boolean, w: String = "")
@kase class CPreToplevelPostcomp[T](x: T, y: Int = 2)(z: Boolean, w: String = "")
@placebo object CPreToplevelPostcomp

class KasePlaceboClass {
  def assertEquals(a: Any, b: Any): Unit = { assert(a == b, s"$a != $b") }

  val objects = scala.collection.mutable.ListBuffer[Any]()
  objects += CPreToplevelNocomp(42)(true)
  objects += CPreToplevelPrecomp(42)(true)
  objects += CPreToplevelPostcomp(42)(true)
  objects += CPostToplevelNocomp(42)(true)
  objects += CPostToplevelPrecomp(42)(true)
  objects += CPostToplevelPostcomp(42)(true)

  @kase class CPreMemberNocomp[T](x: T, y: Int = 2)(z: Boolean, w: String = "")
  @placebo object CPreMemberPrecomp
  @kase class CPreMemberPrecomp[T](x: T, y: Int = 2)(z: Boolean, w: String = "")
  @kase class CPreMemberPostcomp[T](x: T, y: Int = 2)(z: Boolean, w: String = "")
  @placebo object CPreMemberPostcomp
  objects += CPreMemberNocomp(42)(true)
  objects += CPreMemberPrecomp(42)(true)
  objects += CPreMemberPostcomp(42)(true)
  objects += CPostMemberNocomp(42)(true)
  objects += CPostMemberPrecomp(42)(true)
  objects += CPostMemberPostcomp(42)(true)
  @kase class CPostMemberNocomp[T](x: T, y: Int = 2)(z: Boolean, w: String = "")
  @placebo object CPostMemberPrecomp
  @kase class CPostMemberPrecomp[T](x: T, y: Int = 2)(z: Boolean, w: String = "")
  @kase class CPostMemberPostcomp[T](x: T, y: Int = 2)(z: Boolean, w: String = "")
  @placebo object CPostMemberPostcomp

  // @Test
  def combo: Unit = {
    @kase class CPreLocalNocomp[T](x: T, y: Int = 2)(z: Boolean, w: String = "")
    @placebo object CPreLocalPrecomp
    @kase class CPreLocalPrecomp[T](x: T, y: Int = 2)(z: Boolean, w: String = "")
    @kase class CPreLocalPostcomp[T](x: T, y: Int = 2)(z: Boolean, w: String = "")
    @placebo object CPreLocalPostcomp
    objects += CPreLocalNocomp(42)(true)
    objects += CPreLocalPrecomp(42)(true)
    objects += CPreLocalPostcomp(42)(true)
    objects += CPostLocalNocomp(42)(true)
    objects += CPostLocalPrecomp(42)(true)
    objects += CPostLocalPostcomp(42)(true)
    @kase class CPostLocalNocomp[T](x: T, y: Int = 2)(z: Boolean, w: String = "")
    @placebo object CPostLocalPrecomp
    @kase class CPostLocalPrecomp[T](x: T, y: Int = 2)(z: Boolean, w: String = "")
    @kase class CPostLocalPostcomp[T](x: T, y: Int = 2)(z: Boolean, w: String = "")
    @placebo object CPostLocalPostcomp

    assertEquals(objects.mkString("\n"), """
      |CPreToplevelNocomp(42,2)
      |CPreToplevelPrecomp(42,2)
      |CPreToplevelPostcomp(42,2)
      |CPostToplevelNocomp(42,2)
      |CPostToplevelPrecomp(42,2)
      |CPostToplevelPostcomp(42,2)
      |CPreMemberNocomp(42,2)
      |CPreMemberPrecomp(42,2)
      |CPreMemberPostcomp(42,2)
      |CPostMemberNocomp(42,2)
      |CPostMemberPrecomp(42,2)
      |CPostMemberPostcomp(42,2)
      |CPreLocalNocomp(42,2)
      |CPreLocalPrecomp(42,2)
      |CPreLocalPostcomp(42,2)
      |CPostLocalNocomp(42,2)
      |CPostLocalPrecomp(42,2)
      |CPostLocalPostcomp(42,2)
    """.trim.stripMargin)
  }
}

@kase class CPostToplevelNocomp[T](x: T, y: Int = 2)(z: Boolean, w: String = "")
@placebo object CPostToplevelPrecomp
@kase class CPostToplevelPrecomp[T](x: T, y: Int = 2)(z: Boolean, w: String = "")
@kase class CPostToplevelPostcomp[T](x: T, y: Int = 2)(z: Boolean, w: String = "")
@placebo object CPostToplevelPostcomp
