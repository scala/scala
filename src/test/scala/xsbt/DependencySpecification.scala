package xsbt

import xsbti.TestCallback.ExtractedClassDependencies
import sbt.internal.util.UnitSpec

class DependencySpecification extends UnitSpec {

  "Dependency phase" should "extract class dependencies from public members" in {
    val classDependencies = extractClassDependenciesPublic
    val memberRef = classDependencies.memberRef
    val inheritance = classDependencies.inheritance
    assert(memberRef("A") === Set.empty)
    assert(inheritance("A") === Set.empty)
    assert(memberRef("B") === Set("A", "D"))
    assert(inheritance("B") === Set("D"))
    assert(memberRef("C") === Set("A"))
    assert(inheritance("C") === Set.empty)
    assert(memberRef("D") === Set.empty)
    assert(inheritance("D") === Set.empty)
    assert(memberRef("E") === Set.empty)
    assert(inheritance("E") === Set.empty)
    assert(memberRef("F") === Set("A", "B", "D", "E", "G", "C")) // C is the underlying type of MyC
    assert(inheritance("F") === Set("A", "E"))
    assert(memberRef("H") === Set("B", "E", "G"))
    // aliases and applied type constructors are expanded so we have inheritance dependency on B
    assert(inheritance("H") === Set("B", "E"))
  }

  it should "extract class dependencies from local members" in {
    val classDependencies = extractClassDependenciesLocal
    val memberRef = classDependencies.memberRef
    val inheritance = classDependencies.inheritance
    val localInheritance = classDependencies.localInheritance
    assert(memberRef("A") === Set.empty)
    assert(inheritance("A") === Set.empty)
    assert(memberRef("B") === Set.empty)
    assert(inheritance("B") === Set.empty)
    assert(memberRef("C.Inner1") === Set("A"))
    assert(inheritance("C.Inner1") === Set("A"))
    assert(memberRef("D") === Set("B"))
    assert(inheritance("D") === Set.empty)
    assert(localInheritance("D") === Set("B"))
    assert(memberRef("E") === Set("B"))
    assert(inheritance("E") === Set.empty)
    assert(localInheritance("E") === Set("B"))
  }

  it should "extract class dependencies with trait as first parent" in {
    val classDependencies = extractClassDependenciesTraitAsFirstPatent
    val memberRef = classDependencies.memberRef
    val inheritance = classDependencies.inheritance
    assert(memberRef("A") === Set.empty)
    assert(inheritance("A") === Set.empty)
    assert(memberRef("B") === Set("A"))
    assert(inheritance("B") === Set("A"))
    // verify that memberRef captures the oddity described in documentation of `Relations.inheritance`
    // we are mainly interested whether dependency on A is captured in `memberRef` relation so
    // the invariant that says that memberRef is superset of inheritance relation is preserved
    assert(memberRef("C") === Set("A", "B"))
    assert(inheritance("C") === Set("A", "B"))
    // same as above but indirect (C -> B -> A), note that only A is visible here
    assert(memberRef("D") === Set("A", "C"))
    assert(inheritance("D") === Set("A", "C"))
  }

  it should "extract class dependencies from macro arguments" in {
    val classDependencies = extractClassDependenciesFromMacroArgument
    val memberRef = classDependencies.memberRef
    val inheritance = classDependencies.inheritance

    assert(memberRef("A") === Set("B", "C"))
    assert(inheritance("A") === Set.empty)
    assert(memberRef("B") === Set.empty)
    assert(inheritance("B") === Set.empty)
    assert(memberRef("C") === Set.empty)
    assert(inheritance("C") === Set.empty)
  }

  it should "extract class dependencies from a refinement" in {
    val srcFoo = "object Outer {\n  class Inner { type Xyz }\n\n  type TypeInner = Inner { type Xyz = Int }\n}"
    val srcBar = "object Bar {\n  def bar: Outer.TypeInner = null\n}"

    val compilerForTesting = new ScalaCompilerForUnitTesting(nameHashing = true)
    val classDependencies =
      compilerForTesting.extractDependenciesFromSrcs(srcFoo, srcBar)

    val memberRef = classDependencies.memberRef
    val inheritance = classDependencies.inheritance
    assert(memberRef("Outer") === Set.empty)
    assert(inheritance("Outer") === Set.empty)
    assert(memberRef("Bar") === Set("Outer", "Outer.Inner"))
    assert(inheritance("Bar") === Set.empty)
  }

  it should "extract class dependency on a object correctly" in {
    val srcA =
      """object A {
        |   def foo = { B; () }
        |}""".stripMargin
    val srcB = "object B"

    val compilerForTesting = new ScalaCompilerForUnitTesting(nameHashing = true)
    val classDependencies =
      compilerForTesting.extractDependenciesFromSrcs(srcA, srcB)

    val memberRef = classDependencies.memberRef
    val inheritance = classDependencies.inheritance
    assert(memberRef("A") === Set("B"))
    assert(inheritance("A") === Set.empty)
    assert(memberRef("B") === Set.empty)
    assert(inheritance("B") === Set.empty)
  }

  it should "handle top level import dependencies" in {
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

    val compilerForTesting = new ScalaCompilerForUnitTesting(nameHashing = true)
    val deps = compilerForTesting.extractDependenciesFromSrcs(srcA, srcB, srcC, srcD, srcE, srcF, srcG, srcH).memberRef

    assert(deps("A") === Set.empty)
    assert(deps("B") === Set("abc.A", "abc.A.Inner"))
    assert(deps("C") === Set("abc.A", "abc.A2"))
    assert(deps("D") === Set("abc.A2"))
    assert(deps("E") === Set("abc.A"))
    assert(deps("F") === Set.empty)
    assert(deps("foo.bar.G") === Set("abc.A"))
    assert(deps("H") === Set("abc.A"))
  }

  private def extractClassDependenciesPublic: ExtractedClassDependencies = {
    val srcA = "class A"
    val srcB = "class B extends D[A]"
    val srcC = """|class C {
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

    val compilerForTesting = new ScalaCompilerForUnitTesting(nameHashing = true)
    val classDependencies = compilerForTesting.extractDependenciesFromSrcs(srcA, srcB, srcC, srcD, srcE, srcF, srcG,
      srcH)
    classDependencies
  }

  private def extractClassDependenciesLocal: ExtractedClassDependencies = {
    val srcA = "class A"
    val srcB = "class B"
    val srcC = "class C { private class Inner1 extends A }"
    val srcD = "class D { def foo: Unit = { class Inner2 extends B } }"
    val srcE = "class E { def foo: Unit = { new B {} } }"

    val compilerForTesting = new ScalaCompilerForUnitTesting(nameHashing = true)
    val classDependencies =
      compilerForTesting.extractDependenciesFromSrcs(srcA, srcB, srcC, srcD, srcE)
    classDependencies
  }

  private def extractClassDependenciesTraitAsFirstPatent: ExtractedClassDependencies = {
    val srcA = "class A"
    val srcB = "trait B extends A"
    val srcC = "trait C extends B"
    val srcD = "class D extends C"

    val compilerForTesting = new ScalaCompilerForUnitTesting(nameHashing = true)
    val classDependencies =
      compilerForTesting.extractDependenciesFromSrcs(srcA, srcB, srcC, srcD)
    classDependencies
  }

  private def extractClassDependenciesFromMacroArgument: ExtractedClassDependencies = {
    val srcA = "class A { println(B.printTree(C.foo)) }"
    val srcB = """
			|import scala.language.experimental.macros
			|import scala.reflect.macros._
			|object B {
			|  def printTree(arg: Any) = macro printTreeImpl
			|  def printTreeImpl(c: Context)(arg: c.Expr[Any]): c.Expr[String] = {
			|    val argStr = arg.tree.toString
			|    val literalStr = c.universe.Literal(c.universe.Constant(argStr))
			|    c.Expr[String](literalStr)
			|  }
			|}""".stripMargin
    val srcC = "object C { val foo = 1 }"

    val compilerForTesting = new ScalaCompilerForUnitTesting(nameHashing = true)
    val classDependencies =
      compilerForTesting.extractDependenciesFromSrcs(List(List(srcB, srcC), List(srcA)))
    classDependencies
  }

}
