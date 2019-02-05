package placeboannot.klass

import pkg._

@placebo class CPreToplevelNocomp { override def toString = "CPreToplevelNocomp" }
object CPreToplevelPrecomp { def apply() = new CPreToplevelPrecomp }
@placebo class CPreToplevelPrecomp { override def toString = "CPreToplevelPrecomp" }
@placebo class CPreToplevelPostcomp { override def toString = "CPreToplevelPostcomp" }
object CPreToplevelPostcomp { def apply() = new CPreToplevelPostcomp }

class PlaceboClass {
  def assertEquals(a: Any, b: Any): Unit = { assert(a == b, s"$a != $b") }

  val objects = scala.collection.mutable.ListBuffer[Any]()
  objects += new CPreToplevelNocomp()
  objects += CPreToplevelPrecomp()
  objects += CPreToplevelPostcomp()
  objects += new CPostToplevelNocomp()
  objects += CPostToplevelPrecomp()
  objects += CPostToplevelPostcomp()

  @placebo class CPreMemberNocomp { override def toString = "CPreMemberNocomp" }
  object CPreMemberPrecomp { def apply() = new CPreMemberPrecomp }
  @placebo class CPreMemberPrecomp { override def toString = "CPreMemberPrecomp" }
  @placebo class CPreMemberPostcomp { override def toString = "CPreMemberPostcomp" }
  object CPreMemberPostcomp { def apply() = new CPreMemberPostcomp }
  objects += new CPreMemberNocomp()
  objects += CPreMemberPrecomp()
  objects += CPreMemberPostcomp()
  objects += new CPostMemberNocomp()
  objects += CPostMemberPrecomp()
  objects += CPostMemberPostcomp()
  @placebo class CPostMemberNocomp { override def toString = "CPostMemberNocomp" }
  object CPostMemberPrecomp { def apply() = new CPostMemberPrecomp }
  @placebo class CPostMemberPrecomp { override def toString = "CPostMemberPrecomp" }
  @placebo class CPostMemberPostcomp { override def toString = "CPostMemberPostcomp" }
  object CPostMemberPostcomp { def apply() = new CPostMemberPostcomp }

  // @Test
  def combo: Unit = {
    @placebo class CPreLocalNocomp { override def toString = "CPreLocalNocomp" }
    object CPreLocalPrecomp { def apply() = new CPreLocalPrecomp }
    @placebo class CPreLocalPrecomp { override def toString = "CPreLocalPrecomp" }
    @placebo class CPreLocalPostcomp { override def toString = "CPreLocalPostcomp" }
    object CPreLocalPostcomp { def apply() = new CPreLocalPostcomp }
    objects += new CPreLocalNocomp()
    objects += CPreLocalPrecomp()
    objects += CPreLocalPostcomp()
    objects += new CPostLocalNocomp()
    objects += CPostLocalPrecomp()
    objects += CPostLocalPostcomp()
    @placebo class CPostLocalNocomp { override def toString = "CPostLocalNocomp" }
    object CPostLocalPrecomp { def apply() = new CPostLocalPrecomp }
    @placebo class CPostLocalPrecomp { override def toString = "CPostLocalPrecomp" }
    @placebo class CPostLocalPostcomp { override def toString = "CPostLocalPostcomp" }
    object CPostLocalPostcomp { def apply() = new CPostLocalPostcomp }

    assertEquals(objects.mkString("\n"), """
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

@placebo class CPostToplevelNocomp { override def toString = "CPostToplevelNocomp" }
object CPostToplevelPrecomp { def apply() = new CPostToplevelPrecomp }
@placebo class CPostToplevelPrecomp { override def toString = "CPostToplevelPrecomp" }
@placebo class CPostToplevelPostcomp { override def toString = "CPostToplevelPostcomp" }
object CPostToplevelPostcomp { def apply() = new CPostToplevelPostcomp }
