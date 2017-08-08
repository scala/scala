package scala.tools.nsc.backend.jvm.opt

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.nsc.backend.jvm.BTypes.InternalName
import scala.tools.nsc.backend.jvm.opt.InlineSourceMatcherTest._
import scala.tools.nsc.backend.jvm.opt.InlinerHeuristics._
import scala.tools.testing.BytecodeTesting
import scala.tools.testing.BytecodeTesting._

@RunWith(classOf[JUnit4])
class InlineSourceMatcherTest extends BytecodeTesting {
  import compiler._

  override def compilerArgs = "-opt:l:inline -opt-warnings"
  def setInlineFrom(s: String): Unit = {
    global.settings.optInlineFrom.value = s.split(':').toList
  }

  case class E(regex: String, negated: Boolean = false, terminal: Boolean = true)

  def check(pat: String, expect: E*): InlineSourceMatcher = {
    val m = new InlineSourceMatcher(pat.split(':').toList)
    val es = m.entries
    assertEquals(es.length, expect.length)

    for ((a, e) <- (es, expect).zipped) {
      assertEquals(a.pattern.pattern, e.regex)
      assertEquals(a.negated, e.negated)
      assertEquals(a.terminal, e.terminal)
    }

    m
  }

  @Test
  def matcherTest(): Unit = {
    {
      val m = check("a.D", E("a/D"))
      m.a("a/D")
      m.d("a/C")
      m.d("a.D")
      m.d("D")
      assert(!m.allowFromSources)
    }
    {
      val m = check("!a.D", E("a/D", true, true))
      m.d("a/D")
      m.d("a/C")
    }
    {
      val m = check("a.*", E("a/[^/]*"))
      m.a("a/A")
      m.a("a/alKD@(ßª™˜∆≤$N1")
      m.a("a/") // it's maybe a bit weird that this matches, but doesn't matter in practice, we always check real internal names
      m.d("a//")
      m.d("a//A")
      m.d("A")
      m.d("a/b/A")
    }
    {
      val m = check("a.*:!a.C", E("a/[^/]*", false, false), E("a/C", true, true))
      m.a("a/A")
      m.a("a/CC")
      m.d("a/C")
      m.d("a/b/C")
    }
    {
      val m = check("a.*:!a.*C*", E("a/[^/]*", false, false), E("a/[^/]*C[^/]*", true, true))
      m.a("a/A")
      m.a("a/SDEJAB")
      m.d("a/C")
      m.d("a/baC")
      m.d("a/Cal")
      m.d("a/IENABCEKL")
      m.d("a/AlCmalCu")
    }

    {
      // no entry for **, sets the matcher's `startAllow` boolean
      val m = check("**")
      m.a("")
      m.a("a/b/C")
    }
    {
      val m = check("!**", E(".*", true, true))
      m.d("")
      m.d("a/b/C")
    }
    {
      // no entry for **, sets the matcher's `startAllow` boolean
      val m = check("**:!scala.Predef$:!java.**", E("scala/\\QPredef$\\E", true, true), E("java/.*", true, true))
      m.a("Predef$")
      m.a("skala/Predef$")
      m.a("scala/Predef")
      m.d("scala/Predef$")
      m.a("javax/Swing")
      m.d("java/lang/Object")
      m.d("java/Foo")
    }

    {
      val m = check("a.**.c.D", E("a/(?:.*/|)c/D"))
      m.a("a/c/D")
      m.a("a/b/c/D")
      m.a("a/b/i/a/c/c/c/D")
      m.a("a//c/D")
      m.d("a/D")
      m.d("ac/D")
    }
    {
      val m = check("a**.c.D", E("a.*/c/D"))
      m.a("alpha/c/D")
      m.a("alpa/c/a/c/D")
      m.a("a/c/D")
      m.a("a//c/D")
      m.d("ac/D")
      m.d("alp/ac/D")
    }
    {
      val m = check("a**c.D", E("a.*c/D"))
      m.a("ac/D")
      m.a("a/c/D")
      m.a("alpac/D")
      m.a("a/b/c/D")
    }
    {
      val m = check("**.A", E("(?:.*/|)A"))
      m.a("A")
      m.a("p/A")
      m.a("a/b/c/A")
      m.d("pA")
    }
    {
      val m = check("**.*Util*", E("(?:.*/|)[^/]*Util[^/]*"))
      m.a("Util")
      m.a("SourceUtilTools")
      m.a("/Util")
      m.a("/SUtils")
      m.a("a/b/Util")
      m.a("a/b/Utils")
    }
    {
      val m = check("**.*Util*:!**.AUtil*:a/b/AUtil*",
        E("(?:.*/|)[^/]*Util[^/]*", false, false),
        E("(?:.*/|)AUtil[^/]*", true, false),
        E("a/b/AUtil[^/]*", false, true))
      m.a("a/b/AUtils")
      m.d("a/c/AUtils")
      m.d("AUtils")
      m.a("a/c/SAUtils")
    }

    {
      val m = check("**:!a.*:a.C", E("a/[^/]*", true, false), E("a/C", false, true))
      m.d("a/A")
      m.a("a/C")
      m.a("a/A/K")
      m.a("a/C/K")
      m.d("a/")
    }
    {
      val m = check("**:!**.C:C", E("(?:.*/|)C", true, false), E("C", false, true))
      m.a("C")
      m.d("a/C")
    }
    {
      val m = check("scala.**:<sources>:com.corp.**", E("scala/.*", false, true), E("com/corp/.*", false, true))
      assert(m.allowFromSources)
    }
  }

  @Test
  def inlineFromSameClass(): Unit = {
    val code =
      """class C {
        |  @inline final def f = 1
        |  def t = f
        |}
      """.stripMargin

    def n(): Unit = assertInvoke(getMethod(compileClass(code), "t"), "C", "f")
    def y(): Unit = assertNoInvoke(getMethod(compileClass(code), "t"))

    setInlineFrom(""); n()
    setInlineFrom("C"); y()
    setInlineFrom("**:!**.C"); n()
    setInlineFrom("**:!**.C:C"); y()
  }

  @Test
  def inlineFromPackages(): Unit = {
    val code =
      """package a { class C {
        |  object D { @inline def f = 1 }
        |  @inline final def f = 2
        |}}
        |package b { class E { import a._
        |  def t1(c: C) = c.f
        |  def t2(c: C) = c.D.f
        |}}
      """.stripMargin

    {
      setInlineFrom("")
      val List(_, _, e) = compileClasses(code)
      assertInvoke(getMethod(e, "t1"), "a/C", "f")
      assertInvoke(getMethod(e, "t2"), "a/C$D$", "f")
    }
    {
      setInlineFrom("a.C")
      val List(_, _, e) = compileClasses(code)
      assertNoInvoke(getMethod(e, "t1"))
      assertInvoke(getMethod(e, "t2"), "a/C$D$", "f")
    }
    {
      setInlineFrom("a.C*")
      val List(_, _, e) = compileClasses(code)
      assertNoInvoke(getMethod(e, "t1"))
      assertDoesNotInvoke(getMethod(e, "t2"), "f") // t2 still has an invocation to the getter `D`
    }
    {
      setInlineFrom("a.C*:!a.C*$")
      val List(_, _, e) = compileClasses(code)
      assertNoInvoke(getMethod(e, "t1"))
      assertInvoke(getMethod(e, "t2"), "a/C$D$", "f")
    }
  }

  @Test
  def inlineFromSources(): Unit = {
    val a = "class A { @inline final def f = 1 }"
    val b = "class B { def t(a: A) = a.f }"
    setInlineFrom("<sources>")

    {
      val List(_, cb) = compileClasses(s"$a\n$b")
      assertNoInvoke(getMethod(cb, "t"))
    }
    {
      val List(_, cb) = compileClassesSeparately(List(a, b))
      assertInvoke(getMethod(cb, "t"), "A", "f")
    }
  }
}

object InlineSourceMatcherTest {
  implicit class AssertAllow(val m: InlineSourceMatcher) extends AnyVal {
    def a(internalName: InternalName): Unit = assertTrue(m.allow(internalName))
    def d(internalName: InternalName): Unit = assertFalse(m.allow(internalName))
  }
}