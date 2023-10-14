import scala.tools.partest._
import scala.collection.mutable.LinkedHashMap

object Test extends CompilerTest {
  import global._
  override def extraSettings = super.extraSettings + " -Yrangepos -Ystop-after:parser"
  val tests = LinkedHashMap(
    "class A { def t       = new C()   }" -> (24, 31),
    "class B { def t       = (new C)   }" -> (25, 30),
    "class C { def t       = new C     }" -> (24, 29),
    "class D { def t       = new C().t }" -> (24, 33),
    "class E { def t       = (new C).t }" -> (24, 33),
    "class F { def t(c: C) = c         }" -> (24, 25),
    "class G { def t(c: C) = (c)       }" -> (25, 26),
    "class H { def t(c: C) = c.t       }" -> (24, 27),
    "class I { def t(c: C) = (c).t     }" -> (24, 29),
    "class J { def t[T]: C = (x.t)[C]  }" -> (24, 32),
    "class K { def t(f: F) = (f) t c   }" -> (24, 31),
    "class L { def t(c: C) = (c) t     }" -> (24, 29),
    //                       ^ 24     ^ 33
  )

  override def sources = tests.toList.map(_._1)

  def check(source: String, unit: CompilationUnit): Unit = unit.body foreach {
    case dd: DefDef if dd.name.startsWith("t") =>
      val pos = dd.rhs.pos
      val (start, end) = tests(source)
      assert(pos.start == start, pos.start)
      assert(pos.end == end, pos.end)
    case _ =>
  }
}
