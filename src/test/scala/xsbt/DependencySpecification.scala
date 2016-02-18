package xsbt

import org.junit.runner.RunWith
import xsbti.TestCallback.ExtractedClassDependencies
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class DependencySpecification extends Specification {

  "Extracted source dependencies from public members" in {
    val classDependencies = extractClassDependenciesPublic
    val memberRef = classDependencies.memberRef
    val inheritance = classDependencies.inheritance
    memberRef("A") === Set.empty
    inheritance("A") === Set.empty
    memberRef("B") === Set("A", "D")
    inheritance("B") === Set("D")
    memberRef("C") === Set("A")
    inheritance("C") === Set.empty
    memberRef("D") === Set.empty
    inheritance("D") === Set.empty
    memberRef("E") === Set.empty
    inheritance("E") === Set.empty
    memberRef("F") === Set("A", "B", "D", "E", "G")
    inheritance("F") === Set("A", "E")
    memberRef("H") === Set("B", "E", "G")
    // aliases and applied type constructors are expanded so we have inheritance dependency on B
    inheritance("H") === Set("B", "E")
  }

  "Extracted source dependencies from local members" in {
    val classDependencies = extractClassDependenciesLocal
    val memberRef = classDependencies.memberRef
    val inheritance = classDependencies.inheritance
    val localInheritance = classDependencies.localInheritance
    memberRef("A") === Set.empty
    inheritance("A") === Set.empty
    memberRef("B") === Set.empty
    inheritance("B") === Set.empty
    memberRef("C.Inner1") === Set("A")
    inheritance("C.Inner1") === Set("A")
    memberRef("D") === Set("B")
    inheritance("D") === Set.empty
    localInheritance("D") === Set("B")
    memberRef("E") === Set("B")
    inheritance("E") === Set.empty
    localInheritance("E") === Set("B")
  }

  "Extracted source dependencies with trait as first parent" in {
    val classDependencies = extractClassDependenciesTraitAsFirstPatent
    val memberRef = classDependencies.memberRef
    val inheritance = classDependencies.inheritance
    memberRef("A") === Set.empty
    inheritance("A") === Set.empty
    memberRef("B") === Set("A")
    inheritance("B") === Set("A")
    // verify that memberRef captures the oddity described in documentation of `Relations.inheritance`
    // we are mainly interested whether dependency on A is captured in `memberRef` relation so
    // the invariant that says that memberRef is superset of inheritance relation is preserved
    memberRef("C") === Set("A", "B")
    inheritance("C") === Set("A", "B")
    // same as above but indirect (C -> B -> A), note that only A is visible here
    memberRef("D") === Set("A", "C")
    inheritance("D") === Set("A", "C")
  }

  "Extracted source dependencies from macro arguments" in {
    val classDependencies = extractClassDependenciesFromMacroArgument
    val memberRef = classDependencies.memberRef
    val inheritance = classDependencies.inheritance

    memberRef("A") === Set("B", "C")
    inheritance("A") === Set.empty
    memberRef("B") === Set.empty
    inheritance("B") === Set.empty
    memberRef("C") === Set.empty
    inheritance("C") === Set.empty
  }

  "Extracted class dependencies from refinement" in {
    val srcFoo = "object Outer {\n  class Inner { type Xyz }\n\n  type TypeInner = Inner { type Xyz = Int }\n}"
    val srcBar = "object Bar {\n  def bar: Outer.TypeInner = null\n}"

    val compilerForTesting = new ScalaCompilerForUnitTesting(nameHashing = true)
    val classDependencies =
      compilerForTesting.extractDependenciesFromSrcs(srcFoo, srcBar)

    val memberRef = classDependencies.memberRef
    val inheritance = classDependencies.inheritance
    memberRef("Outer") === Set.empty
    inheritance("Outer") === Set.empty
    memberRef("Bar") === Set("Outer")
    inheritance("Bar") === Set.empty
  }

  "Class dependency on object" in {
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
    memberRef("A") === Set("B")
    inheritance("A") === Set.empty
    memberRef("B") === Set.empty
    inheritance("B") === Set.empty
  }

  "Top level import dependencies" in {
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

    deps("A") === Set.empty
    deps("B") === Set("abc.A", "abc.A.Inner")
    deps("C") === Set("abc.A", "abc.A2")
    deps("D") === Set("abc.A2")
    deps("E") === Set("abc.A")
    deps("F") === Set.empty
    deps("foo.bar.G") === Set("abc.A")
    deps("H") === Set("abc.A")
  }

  private def extractClassDependenciesPublic: ExtractedClassDependencies = {
    val srcA = "class A"
    val srcB = "class B extends D[A]"
    val srcC = """|class C {
		  |  def a: A = null
		  |}""".stripMargin
    val srcD = "class D[T]"
    val srcE = "trait E[T]"
    val srcF = "trait F extends A with E[D[B]] { self: C => }"
    val srcG = "object G { type T[x] = B }"
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
