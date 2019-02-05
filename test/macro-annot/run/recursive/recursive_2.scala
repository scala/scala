// package recursive

import pkg._

@plusTwo class CPreToplevelNocomp { override def toString = "CPreToplevelNocomp" }
object CPreToplevelPrecomp { def apply() = new CPreToplevelPrecomp }
@plusTwo class CPreToplevelPrecomp { override def toString = "CPreToplevelPrecomp" }
@plusTwo class CPreToplevelPostcomp { override def toString = "CPreToplevelPostcomp" }
object CPreToplevelPostcomp { def apply() = new CPreToplevelPostcomp }

object Test extends App {
  def assertEquals(a: Any, b: Any): Unit = { assert(a == b, s"$a != $b") }

  val objects = scala.collection.mutable.ListBuffer[Any]()
  objects += new CPreToplevelNocomp()
  objects += CPreToplevelPrecomp()
  objects += CPreToplevelPostcomp()
  objects += new CPostToplevelNocomp()
  objects += CPostToplevelPrecomp()
  objects += CPostToplevelPostcomp()

  @plusTwo class CPreMemberNocomp { override def toString = "CPreMemberNocomp" }
  object CPreMemberPrecomp { def apply() = new CPreMemberPrecomp }
  @plusTwo class CPreMemberPrecomp { override def toString = "CPreMemberPrecomp" }
  @plusTwo class CPreMemberPostcomp { override def toString = "CPreMemberPostcomp" }
  object CPreMemberPostcomp { def apply() = new CPreMemberPostcomp }
  objects += new CPreMemberNocomp()
  objects += CPreMemberPrecomp()
  objects += CPreMemberPostcomp()
  objects += new CPostMemberNocomp()
  objects += CPostMemberPrecomp()
  objects += CPostMemberPostcomp()
  @plusTwo class CPostMemberNocomp { override def toString = "CPostMemberNocomp" }
  object CPostMemberPrecomp { def apply() = new CPostMemberPrecomp }
  @plusTwo class CPostMemberPrecomp { override def toString = "CPostMemberPrecomp" }
  @plusTwo class CPostMemberPostcomp { override def toString = "CPostMemberPostcomp" }
  object CPostMemberPostcomp { def apply() = new CPostMemberPostcomp }

  // @Test def combo: Unit =
  {
    @plusTwo class CPreLocalNocomp { override def toString = "CPreLocalNocomp" }
    object CPreLocalPrecomp { def apply() = new CPreLocalPrecomp }
    @plusTwo class CPreLocalPrecomp { override def toString = "CPreLocalPrecomp" }
    @plusTwo class CPreLocalPostcomp { override def toString = "CPreLocalPostcomp" }
    object CPreLocalPostcomp { def apply() = new CPreLocalPostcomp }
    objects += new CPreLocalNocomp()
    objects += CPreLocalPrecomp()
    objects += CPreLocalPostcomp()
    objects += new CPostLocalNocomp()
    objects += CPostLocalPrecomp()
    objects += CPostLocalPostcomp()
    @plusTwo class CPostLocalNocomp { override def toString = "CPostLocalNocomp" }
    object CPostLocalPrecomp { def apply() = new CPostLocalPrecomp }
    @plusTwo class CPostLocalPrecomp { override def toString = "CPostLocalPrecomp" }
    @plusTwo class CPostLocalPostcomp { override def toString = "CPostLocalPostcomp" }
    object CPostLocalPostcomp { def apply() = new CPostLocalPostcomp }

    assertEquals(objects.mkString("\n"), """
      |CPreToplevelNocomp+1+1
      |CPreToplevelPrecomp+1+1
      |CPreToplevelPostcomp+1+1
      |CPostToplevelNocomp+1+1
      |CPostToplevelPrecomp+1+1
      |CPostToplevelPostcomp+1+1
      |CPreMemberNocomp+1+1
      |CPreMemberPrecomp+1+1
      |CPreMemberPostcomp+1+1
      |CPostMemberNocomp+1+1
      |CPostMemberPrecomp+1+1
      |CPostMemberPostcomp+1+1
      |CPreLocalNocomp+1+1
      |CPreLocalPrecomp+1+1
      |CPreLocalPostcomp+1+1
      |CPostLocalNocomp+1+1
      |CPostLocalPrecomp+1+1
      |CPostLocalPostcomp+1+1
    """.trim.stripMargin)
  }
}

@plusTwo class CPostToplevelNocomp { override def toString = "CPostToplevelNocomp" }
object CPostToplevelPrecomp { def apply() = new CPostToplevelPrecomp }
@plusTwo class CPostToplevelPrecomp { override def toString = "CPostToplevelPrecomp" }
@plusTwo class CPostToplevelPostcomp { override def toString = "CPostToplevelPostcomp" }
object CPostToplevelPostcomp { def apply() = new CPostToplevelPostcomp }
