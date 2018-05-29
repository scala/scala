package kaseannot.placebo.objekt

import pkg._

@kase object CPreToplevelNocomp
@placebo class CPreToplevelPrecomp
@kase object CPreToplevelPrecomp
@kase object CPreToplevelPostcomp
@placebo class CPreToplevelPostcomp

class KasePlaceboObject {
  def assertEquals(a: Any, b: Any): Unit = { assert(a == b, s"$a != $b") }

  val objects = scala.collection.mutable.ListBuffer[Any]()
  objects += CPreToplevelNocomp
  objects += CPreToplevelPrecomp
  objects += CPreToplevelPostcomp
  objects += CPostToplevelNocomp
  objects += CPostToplevelPrecomp
  objects += CPostToplevelPostcomp

  // TODO: doesn't work in sbt, though does work in the command line
  // @kase object CPreMemberNocomp
  // @placebo class CPreMemberPrecomp
  // @kase object CPreMemberPrecomp
  // @kase object CPreMemberPostcomp
  // @placebo class CPreMemberPostcomp
  // objects += CPreMemberNocomp
  // objects += CPreMemberPrecomp
  // objects += CPreMemberPostcomp
  // objects += CPostMemberNocomp
  // objects += CPostMemberPrecomp
  // objects += CPostMemberPostcomp
  // @kase object CPostMemberNocomp
  // @placebo class CPostMemberPrecomp
  // @kase object CPostMemberPrecomp
  // @kase object CPostMemberPostcomp
  // @placebo class CPostMemberPostcomp

  // @Test
  def combo: Unit = {
    @kase object CPreLocalNocomp
    @placebo class CPreLocalPrecomp
    @kase object CPreLocalPrecomp
    @kase object CPreLocalPostcomp
    @placebo class CPreLocalPostcomp
    objects += CPreLocalNocomp
    objects += CPreLocalPrecomp
    objects += CPreLocalPostcomp
    objects += CPostLocalNocomp
    objects += CPostLocalPrecomp
    objects += CPostLocalPostcomp
    @kase object CPostLocalNocomp
    @placebo class CPostLocalPrecomp
    @kase object CPostLocalPrecomp
    @kase object CPostLocalPostcomp
    @placebo class CPostLocalPostcomp

    assertEquals(objects.mkString("\n"), """
      |CPreToplevelNocomp
      |CPreToplevelPrecomp
      |CPreToplevelPostcomp
      |CPostToplevelNocomp
      |CPostToplevelPrecomp
      |CPostToplevelPostcomp
      |CPreLocalNocomp
      |CPreLocalPrecomp
      |CPreLocalPostcomp
      |CPostLocalNocomp
      |CPostLocalPrecomp
      |CPostLocalPostcomp
    """.trim.stripMargin)
  }
}

@kase object CPostToplevelNocomp
@placebo class CPostToplevelPrecomp
@kase object CPostToplevelPrecomp
@kase object CPostToplevelPostcomp
@placebo class CPostToplevelPostcomp
