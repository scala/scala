package kaseannot.identity.objekt

import pkg._

@kase object CPreToplevelNocomp
@identity class CPreToplevelPrecomp
@kase object CPreToplevelPrecomp
@kase object CPreToplevelPostcomp
@identity class CPreToplevelPostcomp

class KaseIdentityObject {
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
  // @identity class CPreMemberPrecomp
  // @kase object CPreMemberPrecomp
  // @kase object CPreMemberPostcomp
  // @identity class CPreMemberPostcomp
  // objects += CPreMemberNocomp
  // objects += CPreMemberPrecomp
  // objects += CPreMemberPostcomp
  // objects += CPostMemberNocomp
  // objects += CPostMemberPrecomp
  // objects += CPostMemberPostcomp
  // @kase object CPostMemberNocomp
  // @identity class CPostMemberPrecomp
  // @kase object CPostMemberPrecomp
  // @kase object CPostMemberPostcomp
  // @identity class CPostMemberPostcomp

  // @Test
  def combo: Unit = {
    @kase object CPreLocalNocomp
    @identity class CPreLocalPrecomp
    @kase object CPreLocalPrecomp
    @kase object CPreLocalPostcomp
    @identity class CPreLocalPostcomp
    objects += CPreLocalNocomp
    objects += CPreLocalPrecomp
    objects += CPreLocalPostcomp
    objects += CPostLocalNocomp
    objects += CPostLocalPrecomp
    objects += CPostLocalPostcomp
    @kase object CPostLocalNocomp
    @identity class CPostLocalPrecomp
    @kase object CPostLocalPrecomp
    @kase object CPostLocalPostcomp
    @identity class CPostLocalPostcomp

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
@identity class CPostToplevelPrecomp
@kase object CPostToplevelPrecomp
@kase object CPostToplevelPostcomp
@identity class CPostToplevelPostcomp
