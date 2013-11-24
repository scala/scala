package xsbt

import org.junit.runner.RunWith
import xsbti.api.ClassLike
import xsbti.api.Def
import xsbt.api.SameAPI
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

import ScalaCompilerForUnitTesting.ExtractedSourceDependencies

@RunWith(classOf[JUnitRunner])
class DependencySpecification extends Specification {

	"Extracted source dependencies from public members" in {
		val sourceDependencies = extractSourceDependenciesPublic
		val memberRef = sourceDependencies.memberRef
		val inheritance = sourceDependencies.inheritance
		memberRef('A) === Set.empty
		inheritance('A) === Set.empty
		memberRef('B) === Set('A, 'D)
		inheritance('B) === Set('D)
		memberRef('C) === Set('A)
		inheritance('C) === Set.empty
		memberRef('D) === Set.empty
		inheritance('D) === Set.empty
		memberRef('E) === Set.empty
		inheritance('E) === Set.empty
		memberRef('F) === Set('A, 'B, 'C, 'D, 'E)
		inheritance('F) === Set('A, 'E)
		memberRef('H) === Set('B, 'E, 'G)
		// aliases and applied type constructors are expanded so we have inheritance dependency on B
		inheritance('H) === Set('B, 'E)
	}

	"Extracted source dependencies from private members" in {
		val sourceDependencies = extractSourceDependenciesPrivate
		val memberRef = sourceDependencies.memberRef
		val inheritance = sourceDependencies.inheritance
		memberRef('A) === Set.empty
		inheritance('A) === Set.empty
		memberRef('B) === Set.empty
		inheritance('B) === Set.empty
		memberRef('C) === Set('A)
		inheritance('C) === Set('A)
		memberRef('D) === Set('B)
		inheritance('D) === Set('B)
	}

	private def extractSourceDependenciesPublic: ExtractedSourceDependencies = {
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

		val compilerForTesting = new ScalaCompilerForUnitTesting(memberRefAndInheritanceDeps = true)
		val sourceDependencies = compilerForTesting.extractDependenciesFromSrcs('A -> srcA, 'B -> srcB, 'C -> srcC,
			'D -> srcD, 'E -> srcE, 'F -> srcF, 'G -> srcG, 'H -> srcH)
		sourceDependencies
	}

	private def extractSourceDependenciesPrivate: ExtractedSourceDependencies = {
		val srcA = "class A"
		val srcB = "class B"
		val srcC = "class C { private class Inner1 extends A }"
		val srcD = "class D { def foo: Unit = { class Inner2 extends B } }"

		val compilerForTesting = new ScalaCompilerForUnitTesting(memberRefAndInheritanceDeps = true)
		val sourceDependencies =
			compilerForTesting.extractDependenciesFromSrcs('A -> srcA, 'B -> srcB, 'C -> srcC, 'D -> srcD)
		sourceDependencies
	}
}
