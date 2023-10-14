package scala.tools.xsbt

import org.junit.Test
import org.junit.Assert.{assertEquals, assertTrue}

import scala.tools.xsbt.TestCallback.ExtractedClassDependencies

class DependencyTest extends BridgeTesting {

  @Test
  def `Dependency phase should extract class dependencies from public members`(): Unit = {
    val classDependencies = extractClassDependenciesPublic
    val memberRef = classDependencies.memberRef
    val inheritance = classDependencies.inheritance
    assertEquals(memberRef("A"), Set.empty)
    assertEquals(inheritance("A"), Set.empty)
    assertEquals(memberRef("B"), Set("A", "D"))
    assertEquals(inheritance("B"), Set("D"))
    assertEquals(memberRef("C"), Set("A"))
    assertEquals(inheritance("C"), Set.empty)
    assertEquals(memberRef("D"), Set.empty)
    assertEquals(inheritance("D"), Set.empty)
    assertEquals(memberRef("E"), Set.empty)
    assertEquals(inheritance("E"), Set.empty)
    assertEquals(memberRef("F"), Set("A", "B", "D", "E", "G", "C")) // C is the underlying type of MyC
    assertEquals(inheritance("F"), Set("A", "E"))
    assertEquals(memberRef("H"), Set("B", "E", "G"))
    // aliases and applied type constructors are expanded so we have inheritance dependency on B
    assertEquals(inheritance("H"), Set("B", "E"))
  }

  @Test
  def `Dependency phase should extract class dependencies from local members`(): Unit = {
    val classDependencies = extractClassDependenciesLocal
    val memberRef = classDependencies.memberRef
    val inheritance = classDependencies.inheritance
    val localInheritance = classDependencies.localInheritance
    assertEquals(memberRef("A"), Set.empty)
    assertEquals(inheritance("A"), Set.empty)
    assertEquals(memberRef("B"), Set.empty)
    assertEquals(inheritance("B"), Set.empty)
    assertEquals(memberRef("C.Inner1"), Set("A"))
    assertEquals(inheritance("C.Inner1"), Set("A"))
    assertEquals(memberRef("D"), Set("B"))
    assertEquals(inheritance("D"), Set.empty)
    assertEquals(localInheritance("D"), Set("B"))
    assertEquals(memberRef("E"), Set("B"))
    assertEquals(inheritance("E"), Set.empty)
    assertEquals(localInheritance("E"), Set("B"))
  }

  @Test
  def `Dependency phase should extract class dependencies with trait as first parent`(): Unit = {
    val classDependencies = extractClassDependenciesTraitAsFirstPatent
    val memberRef = classDependencies.memberRef
    val inheritance = classDependencies.inheritance
    assertEquals(memberRef("A"), Set.empty)
    assertEquals(inheritance("A"), Set.empty)
    assertEquals(memberRef("B"), Set("A"))
    assertEquals(inheritance("B"), Set("A"))
    // verify that memberRef captures the oddity described in documentation of `Relations.inheritance`
    // we are mainly interested whether dependency on A is captured in `memberRef` relation so
    // the invariant that says that memberRef is superset of inheritance relation is preserved
    assertEquals(memberRef("C"), Set("A", "B"))
    assertEquals(inheritance("C"), Set("A", "B"))
    // same as above but indirect (C -> B -> A), note that only A is visible here
    assertEquals(memberRef("D"), Set("A", "C"))
    assertEquals(inheritance("D"), Set("A", "C"))
  }

  @Test
  def `Dependency phase should extract class dependencies from macro arguments`(): Unit = {
    val binaryDependencies = extractBinaryDependenciesFromMacroArgument
    val memberRef = binaryDependencies.memberRef
    Set("B$", "C$").foreach(k => assertTrue(k, memberRef("A").contains(k)))
  }

  @Test
  def `Dependency phase should extract class dependencies from a refinement`(): Unit = {
    val srcFoo =
      "object Outer {\n  class Inner { type Xyz }\n\n  type TypeInner = Inner { type Xyz = Int }\n}"
    val srcBar = "object Bar {\n  def bar: Outer.TypeInner = null\n}"
    val classDependencies = extractDependenciesFromSrcs(srcFoo, srcBar)

    val memberRef = classDependencies.memberRef
    val inheritance = classDependencies.inheritance
    assertEquals(memberRef("Outer"), Set.empty)
    assertEquals(inheritance("Outer"), Set.empty)
    assertEquals(memberRef("Bar"), Set("Outer", "Outer.Inner"))
    assertEquals(inheritance("Bar"), Set.empty)
  }

  @Test
  def `Dependency phase should extract class dependency on a object correctly`(): Unit = {
    val srcA =
      """object A {
        |   def foo = { B; () }
        |}""".stripMargin
    val srcB = "object B"

    val classDependencies = extractDependenciesFromSrcs(srcA, srcB)

    val memberRef = classDependencies.memberRef
    val inheritance = classDependencies.inheritance
    assertEquals(memberRef("A"), Set("B"))
    assertEquals(inheritance("A"), Set.empty)
    assertEquals(memberRef("B"), Set.empty)
    assertEquals(inheritance("B"), Set.empty)
  }

  @Test
  def `Dependency phase should extract class dependency from 'classOf' literal`(): Unit = {
    val srcA =
      """object A {
        |   print(classOf[B])
        |}""".stripMargin
    val srcB = "class B"

    val classDependencies = extractDependenciesFromSrcs(srcA, srcB)

    val memberRef = classDependencies.memberRef
    val inheritance = classDependencies.inheritance
    assertEquals(memberRef("A"), Set("B"))
    assertEquals(inheritance("A"), Set.empty)
    assertEquals(memberRef("B"), Set.empty)
    assertEquals(inheritance("B"), Set.empty)
  }

  @Test
  def `Dependency phase should handle top level import dependencies`(): Unit = {
    val srcA =
      """
        |package abc
        |object A {
        |  class Inner
        |}
        |class A2""".stripMargin
    val srcB = "import abc.A; import abc.A.Inner; class B"
    val srcC = "import abc.{A, A2}; class C"
    val srcD = "import abc.{A2 => Foo}; class D"
    val srcE = "import abc.A._; class E"
    val srcF = "import abc._; class F"
    val srcG =
      """|package foo {
         |  package bar {
         |    import abc.A
         |    class G
         |  }
         |}
    """.stripMargin
    val srcH = "class H { import abc.A }"

    val deps = extractDependenciesFromSrcs(srcA, srcB, srcC, srcD, srcE, srcF, srcG, srcH).memberRef

    assertEquals(deps("A"), Set.empty)
    assertEquals(deps("B"), Set("abc.A", "abc.A.Inner"))
    assertEquals(deps("C"), Set("abc.A", "abc.A2"))
    assertEquals(deps("D"), Set("abc.A2"))
    assertEquals(deps("E"), Set("abc.A"))
    assertEquals(deps("F"), Set.empty)
    assertEquals(deps("foo.bar.G"), Set("abc.A"))
    assertEquals(deps("H"), Set("abc.A"))
  }

  private def extractClassDependenciesPublic: ExtractedClassDependencies = {
    val srcA = "class A"
    val srcB = "class B extends D[A]"
    val srcC =
      """|class C {
         |  def a: A = null
         |}""".stripMargin
    val srcD = "class D[T]"
    val srcE = "trait E[T]"
    val srcF = "trait F extends A with E[D[B]] { self: G.MyC => }"
    val srcG = "object G { type T[x] = B ; type MyC = C }"
    // T is a type constructor [x]B
    // B extends D
    // E verifies the core type gets pulled out
    val srcH = "trait H extends G.T[Int] with (E[Int] @unchecked)"

    val classDependencies =
      extractDependenciesFromSrcs(srcA, srcB, srcC, srcD, srcE, srcF, srcG, srcH)
    classDependencies
  }

  private def extractClassDependenciesLocal: ExtractedClassDependencies = {
    val srcA = "class A"
    val srcB = "class B"
    val srcC = "class C { private class Inner1 extends A }"
    val srcD = "class D { def foo: Unit = { class Inner2 extends B } }"
    val srcE = "class E { def foo: Unit = { new B {} } }"

    val classDependencies =
      extractDependenciesFromSrcs(srcA, srcB, srcC, srcD, srcE)
    classDependencies
  }

  private def extractClassDependenciesTraitAsFirstPatent: ExtractedClassDependencies = {
    val srcA = "class A"
    val srcB = "trait B extends A"
    val srcC = "trait C extends B"
    val srcD = "class D extends C"

    val classDependencies =
      extractDependenciesFromSrcs(srcA, srcB, srcC, srcD)
    classDependencies
  }

  private def extractBinaryDependenciesFromMacroArgument: ExtractedClassDependencies = {
    val srcA = "class A { println(B.printTree(C.foo)) }"
    val srcB =
      """
        |import scala.language.experimental.macros
        |import scala.reflect.macros.blackbox._
        |object B {
        |  def printTree(arg: Any): String = macro printTreeImpl
        |  def printTreeImpl(c: Context)(arg: c.Expr[Any]): c.Expr[String] = {
        |    val argStr = arg.tree.toString
        |    val literalStr = c.universe.Literal(c.universe.Constant(argStr))
        |    c.Expr[String](literalStr)
        |  }
        |}""".stripMargin
    val srcC = "object C { val foo = 1 }"

    val binaryDependencies =
      extractBinaryDependenciesFromSrcss(List(List(srcB, srcC), List(srcA)))
    binaryDependencies
  }
}
