package xsbt

import org.junit.runner.RunWith
import xsbti.api.ClassLike
import xsbti.api.Def
import xsbt.api.SameAPI
import sbt.internal.util.UnitSpec

import ScalaCompilerForUnitTesting.ExtractedSourceDependencies

class DependencySpecification extends UnitSpec {

  "Dependency phase" should "extract source dependencies from public members" in {
    val sourceDependencies = extractSourceDependenciesPublic
    val memberRef = sourceDependencies.memberRef
    val inheritance = sourceDependencies.inheritance
    assert(memberRef('A) === Set.empty)
    assert(inheritance('A) === Set.empty)
    assert(memberRef('B) === Set('A, 'D))
    assert(inheritance('B) === Set('D))
    assert(memberRef('C) === Set('A))
    assert(inheritance('C) === Set.empty)
    assert(memberRef('D) === Set.empty)
    assert(inheritance('D) === Set.empty)
    assert(memberRef('E) === Set.empty)
    assert(inheritance('E) === Set.empty)
    assert(memberRef('F) === Set('A, 'B, 'C, 'D, 'E, 'G))
    assert(inheritance('F) === Set('A, 'E))
    assert(memberRef('H) === Set('B, 'E, 'G))
    // aliases and applied type constructors are expanded so we have inheritance dependency on B
    assert(inheritance('H) === Set('B, 'E))
  }

  it should "extract source dependencies from private members" in {
    val sourceDependencies = extractSourceDependenciesPrivate
    val memberRef = sourceDependencies.memberRef
    val inheritance = sourceDependencies.inheritance
    assert(memberRef('A) === Set.empty)
    assert(inheritance('A) === Set.empty)
    assert(memberRef('B) === Set.empty)
    assert(inheritance('B) === Set.empty)
    assert(memberRef('C) === Set('A))
    assert(inheritance('C) === Set('A))
    assert(memberRef('D) === Set('B))
    assert(inheritance('D) === Set('B))
  }

  it should "extract source dependencies with trait as first parent" in {
    val sourceDependencies = extractSourceDependenciesTraitAsFirstPatent
    val memberRef = sourceDependencies.memberRef
    val inheritance = sourceDependencies.inheritance
    assert(memberRef('A) === Set.empty)
    assert(inheritance('A) === Set.empty)
    assert(memberRef('B) === Set('A))
    assert(inheritance('B) === Set('A))
    // verify that memberRef captures the oddity described in documentation of `Relations.inheritance`
    // we are mainly interested whether dependency on A is captured in `memberRef` relation so
    // the invariant that says that memberRef is superset of inheritance relation is preserved
    assert(memberRef('C) === Set('A, 'B))
    assert(inheritance('C) === Set('A, 'B))
    // same as above but indirect (C -> B -> A), note that only A is visible here
    assert(memberRef('D) === Set('A, 'C))
    assert(inheritance('D) === Set('A, 'C))
  }

  it should "extract source dependencies from macro arguments" in {
    val sourceDependencies = extractSourceDependenciesFromMacroArgument
    val memberRef = sourceDependencies.memberRef
    val inheritance = sourceDependencies.inheritance

    assert(memberRef('A) === Set('B, 'C))
    assert(inheritance('A) === Set.empty)
    assert(memberRef('B) === Set.empty)
    assert(inheritance('B) === Set.empty)
    assert(memberRef('C) === Set.empty)
    assert(inheritance('C) === Set.empty)
  }

  private def extractSourceDependenciesPublic: ExtractedSourceDependencies = {
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
    val sourceDependencies = compilerForTesting.extractDependenciesFromSrcs('A -> srcA, 'B -> srcB, 'C -> srcC,
      'D -> srcD, 'E -> srcE, 'F -> srcF, 'G -> srcG, 'H -> srcH)
    sourceDependencies
  }

  private def extractSourceDependenciesPrivate: ExtractedSourceDependencies = {
    val srcA = "class A"
    val srcB = "class B"
    val srcC = "class C { private class Inner1 extends A }"
    val srcD = "class D { def foo: Unit = { class Inner2 extends B } }"

    val compilerForTesting = new ScalaCompilerForUnitTesting(nameHashing = true)
    val sourceDependencies =
      compilerForTesting.extractDependenciesFromSrcs('A -> srcA, 'B -> srcB, 'C -> srcC, 'D -> srcD)
    sourceDependencies
  }

  private def extractSourceDependenciesTraitAsFirstPatent: ExtractedSourceDependencies = {
    val srcA = "class A"
    val srcB = "trait B extends A"
    val srcC = "trait C extends B"
    val srcD = "class D extends C"

    val compilerForTesting = new ScalaCompilerForUnitTesting(nameHashing = true)
    val sourceDependencies =
      compilerForTesting.extractDependenciesFromSrcs('A -> srcA, 'B -> srcB, 'C -> srcC, 'D -> srcD)
    sourceDependencies
  }

  private def extractSourceDependenciesFromMacroArgument: ExtractedSourceDependencies = {
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
    val sourceDependencies =
      compilerForTesting.extractDependenciesFromSrcs(List(Map('B -> srcB, 'C -> srcC), Map('A -> srcA)))
    sourceDependencies
  }

}
