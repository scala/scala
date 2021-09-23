import scala.tools.partest._

object Test extends DirectTest {
  val code: String = """
package a.b.c.d.e.f.g.h.i.j.k.l.m.n.o.p.q.r.s.t.u.v.w.x.y {
  object z {
    type Type
    implicitly[Type]
  }
}
"""

  def show(): Unit = {
    def run(modules: Option[Int]): Unit =
      compileString(newCompiler((
        "-Vimplicits" +: modules.toSeq.flatMap(i => Seq("-Vimplicits-max-modules", i.toString))
      ):_*))(code.trim)

    run(None)
    run(Some(0))
    run(Some(1))
    run(Some(2))
    run(Some(3))
    run(Some(11))
    run(Some(Int.MaxValue))
  }
}
