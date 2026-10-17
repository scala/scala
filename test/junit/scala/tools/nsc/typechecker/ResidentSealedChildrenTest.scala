package scala.tools.nsc.typechecker

import org.junit.Assert.assertEquals
import org.junit.Test

import scala.tools.testkit.BytecodeTesting

// A top-level class symbol is reused when a later Run on the same Global redefines it;
// its sealed children must not leak from earlier runs.
class ResidentSealedChildrenTest {
  val compiler = BytecodeTesting.newCompiler(extraArgs = "-Ystop-after:patmat")
  import compiler.global._

  def compile(files: (String, String)*): List[String] = {
    val run = compiler.newRun()
    run.compileSources(files.toList.map { case (n, c) => BytecodeTesting.makeSourceFile(c, n) })
    reporter.asInstanceOf[scala.tools.nsc.reporters.StoreReporter].infos.toList.map(_.msg)
  }
  def compile(fileName: String, code: String): List[String] = compile(fileName -> code)
  def T = rootMirror.EmptyPackageClass.info.decl(TypeName("T"))
  def childNames = T.sealedChildren.map(_.name.toString).toList.sorted

  @Test def shrinkingSealedHierarchy(): Unit = {
    val warns1 = compile("a.scala",
      """sealed trait T
        |final case class A() extends T
        |final case class B() extends T
        |object O { def f(t: T) = t match { case A() => 1; case B() => 2 } }""".stripMargin)
    assertEquals(Nil, warns1)
    assertEquals(List("A", "B"), childNames)
    val id1 = T.id

    val warns2 = compile("b.scala",
      """sealed trait T
        |final case class A() extends T
        |object O { def f(t: T) = t match { case A() => 1 } }""".stripMargin)
    assertEquals(id1, T.id) // symbol reused by the namer
    assertEquals(List("A"), childNames)
    assertEquals(Nil, warns2)
  }

  // Permitted subclasses of a Java sealed class may live in other files, and they register
  // themselves when their own file is compiled, so recompiling only the parent must keep them.
  @Test def javaPermitsInOtherFiles(): Unit = {
    val p = "P.java" -> "public sealed interface P permits A, B {}"
    val s = "S.scala" -> "object S { def f(p: P) = p match { case _: A => 1; case _: B => 2 } }"
    def P = rootMirror.EmptyPackageClass.info.decl(TypeName("P"))
    assertEquals(Nil, compile(p, "A.java" -> "public final class A implements P {}", "B.java" -> "public final class B implements P {}", s))
    assertEquals(List("A", "B"), P.sealedChildren.map(_.name.toString).toList.sorted)
    assertEquals(Nil, compile(p, s))
    assertEquals(List("A", "B"), P.sealedChildren.map(_.name.toString).toList.sorted)
  }

  // A permitted subclass that is recompiled and no longer extends the parent must be deregistered.
  @Test def javaPermitsChildLeaves(): Unit = {
    def P = rootMirror.EmptyPackageClass.info.decl(TypeName("P"))
    compile("P.java" -> "public sealed interface P permits A, B {}", "A.java" -> "public final class A implements P {}", "B.java" -> "public final class B implements P {}",
      "S.scala" -> "object S { def f(p: P) = p match { case _: A => 1; case _: B => 2 } }")
    assertEquals(List("A", "B"), P.sealedChildren.map(_.name.toString).toList.sorted)
    val warns = compile("P.java" -> "public sealed interface P permits B {}", "A.java" -> "public final class A {}",
      "S.scala" -> "object S { def f(p: P) = p match { case _: B => 2 } }")
    assertEquals(List("B"), P.sealedChildren.map(_.name.toString).toList.sorted)
    assertEquals(Nil, warns)
  }

  @Test def sameFileShrinks(): Unit = {
    compile("a.scala" -> "sealed trait T; case class A() extends T; case object D extends T")
    assertEquals(List("A", "D"), childNames)
    compile("a.scala" -> "sealed trait T; case class A() extends T")
    assertEquals(List("A"), childNames)
  }
}
