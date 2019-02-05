package placeboannot.objekt

import pkg._

@placebo object CPreToplevelNocomp { override def toString = "CPreToplevelNocomp" }
class CPreToplevelPrecomp
@placebo object CPreToplevelPrecomp { override def toString = "CPreToplevelPrecomp" }
@placebo object CPreToplevelPostcomp { override def toString = "CPreToplevelPostcomp" }
class CPreToplevelPostcomp

class PlaceboClass {
  def assertEquals(a: Any, b: Any): Unit = { assert(a == b, s"$a != $b") }

  val classs = scala.collection.mutable.ListBuffer[Any]()
  classs += CPreToplevelNocomp
  classs += CPreToplevelPrecomp
  classs += CPreToplevelPostcomp
  classs += CPostToplevelNocomp
  classs += CPostToplevelPrecomp
  classs += CPostToplevelPostcomp

  @placebo object CPreMemberNocomp { override def toString = "CPreMemberNocomp" }
  class CPreMemberPrecomp
  @placebo object CPreMemberPrecomp { override def toString = "CPreMemberPrecomp" }
  @placebo object CPreMemberPostcomp { override def toString = "CPreMemberPostcomp" }
  class CPreMemberPostcomp
  classs += CPreMemberNocomp
  classs += CPreMemberPrecomp
  classs += CPreMemberPostcomp
  classs += CPostMemberNocomp
  classs += CPostMemberPrecomp
  classs += CPostMemberPostcomp
  @placebo object CPostMemberNocomp { override def toString = "CPostMemberNocomp" }
  class CPostMemberPrecomp
  @placebo object CPostMemberPrecomp { override def toString = "CPostMemberPrecomp" }
  @placebo object CPostMemberPostcomp { override def toString = "CPostMemberPostcomp" }
  class CPostMemberPostcomp

  // @Test
  def combo: Unit = {
    @placebo object CPreLocalNocomp { override def toString = "CPreLocalNocomp" }
    class CPreLocalPrecomp
    @placebo object CPreLocalPrecomp { override def toString = "CPreLocalPrecomp" }
    @placebo object CPreLocalPostcomp { override def toString = "CPreLocalPostcomp" }
    class CPreLocalPostcomp
    classs += CPreLocalNocomp
    classs += CPreLocalPrecomp
    classs += CPreLocalPostcomp
    classs += CPostLocalNocomp
    classs += CPostLocalPrecomp
    classs += CPostLocalPostcomp
    @placebo object CPostLocalNocomp { override def toString = "CPostLocalNocomp" }
    class CPostLocalPrecomp
    @placebo object CPostLocalPrecomp { override def toString = "CPostLocalPrecomp" }
    @placebo object CPostLocalPostcomp { override def toString = "CPostLocalPostcomp" }
    class CPostLocalPostcomp

    assertEquals(classs.mkString("\n"), """
      |CPreToplevelNocomp
      |CPreToplevelPrecomp
      |CPreToplevelPostcomp
      |CPostToplevelNocomp
      |CPostToplevelPrecomp
      |CPostToplevelPostcomp
      |CPreMemberNocomp
      |CPreMemberPrecomp
      |CPreMemberPostcomp
      |CPostMemberNocomp
      |CPostMemberPrecomp
      |CPostMemberPostcomp
      |CPreLocalNocomp
      |CPreLocalPrecomp
      |CPreLocalPostcomp
      |CPostLocalNocomp
      |CPostLocalPrecomp
      |CPostLocalPostcomp
    """.trim.stripMargin)
  }
}

@placebo object CPostToplevelNocomp { override def toString = "CPostToplevelNocomp" }
class CPostToplevelPrecomp
@placebo object CPostToplevelPrecomp { override def toString = "CPostToplevelPrecomp" }
@placebo object CPostToplevelPostcomp { override def toString = "CPostToplevelPostcomp" }
class CPostToplevelPostcomp
