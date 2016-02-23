package xsbt

import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

import org.specs2.mutable.Specification

@RunWith(classOf[JUnitRunner])
class ExtractUsedNamesSpecification extends Specification {

  /**
   * Standard names that appear in every compilation unit that has any class
   * definition.
   */
  private val standardNames = Set(
    // AnyRef is added as default parent of a class
    "scala", "AnyRef",
    // class receives a default constructor which is internally called "<init>"
    "<init>")

  "imported name" in {
    val src = """
			|package a { class A }
			|package b {
			|	import a.{A => A2}
			|}""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting(nameHashing = true)
    val usedNames = compilerForTesting.extractUsedNamesFromSrc(src)
    val expectedNames = standardNames ++ Set("a", "A", "A2", "b")
    usedNames === expectedNames
  }

  // test covers https://github.com/gkossakowski/sbt/issues/6
  "names in type tree" in {
    val srcA = """|
			|package a {
			|  class A {
			|    class C { class D }
			|  }
			|  class B[T]
			|  class BB
			|}""".stripMargin
    val srcB = """|
			|package b {
			|	abstract class X {
			|     def foo: a.A#C#D
			|     def bar: a.B[a.BB]
			|   }
			|}""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting(nameHashing = true)
    val usedNames = compilerForTesting.extractUsedNamesFromSrc(srcA, srcB)
    val expectedNames = standardNames ++ Set("a", "A", "B", "C", "D", "b", "X", "BB")
    usedNames === expectedNames
  }

  // test for https://github.com/gkossakowski/sbt/issues/5
  "symbolic names" in {
    val srcA = """|
			|class A {
			|  def `=`: Int = 3
			|}""".stripMargin
    val srcB = """|
			|class B {
			|  def foo(a: A) = a.`=`
			|}""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting(nameHashing = true)
    val usedNames = compilerForTesting.extractUsedNamesFromSrc(srcA, srcB)
    val expectedNames = standardNames ++ Set("A", "a", "B", "=")
    usedNames === expectedNames
  }

  // test for https://github.com/gkossakowski/sbt/issues/3
  "used names from the same compilation unit" in {
    val src = "class A { def foo: Int = 0; def bar: Int = foo }"
    val compilerForTesting = new ScalaCompilerForUnitTesting(nameHashing = true)
    val usedNames = compilerForTesting.extractUsedNamesFromSrc(src)
    val expectedNames = standardNames ++ Set("A", "foo", "Int")
    usedNames === expectedNames
  }

  // pending test for https://issues.scala-lang.org/browse/SI-7173
  "names of constants" in {
    val src = "class A { final val foo = 12; def bar: Int = foo }"
    val compilerForTesting = new ScalaCompilerForUnitTesting(nameHashing = true)
    val usedNames = compilerForTesting.extractUsedNamesFromSrc(src)
    val expectedNames = standardNames ++ Set("A", "foo", "Int")
    usedNames === expectedNames
  }.pendingUntilFixed("Scala's type checker inlines constants so we can't see the original name.")

  // pending test for https://github.com/gkossakowski/sbt/issues/4
  // TODO: we should fix it by having special treatment of `selectDynamic` and `applyDynamic` calls
  "names from method calls on Dynamic" in {
    val srcA = """|import scala.language.dynamics
			|class A extends Dynamic {
			|	def selectDynamic(name: String): Int = name.length
			|}""".stripMargin
    val srcB = "class B { def foo(a: A): Int = a.bla }"
    val compilerForTesting = new ScalaCompilerForUnitTesting(nameHashing = true)
    val usedNames = compilerForTesting.extractUsedNamesFromSrc(srcA, srcB)
    val expectedNames = standardNames ++ Set("B", "A", "a", "Int", "selectDynamic", "bla")
    usedNames === expectedNames
  }.pendingUntilFixed("Call to Dynamic is desugared in type checker so Select nodes is turned into string literal.")

}
