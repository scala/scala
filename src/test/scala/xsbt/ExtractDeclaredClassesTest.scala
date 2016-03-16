package xsbt

import xsbti.api.ClassLike
import xsbti.api.Def
import xsbti.api.Package
import xsbti.api._
import xsbt.api.HashAPI

import sbt.internal.util.UnitSpec

class ExtractDeclaredClassesTest extends UnitSpec {

  "ExtractDeclaredClasses phase" should "handle the default package" in {
    val src = """
			|class A
			|object B
			|""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val declaredClasses = compilerForTesting.extractDeclaredClassesFromSrc(src)
    val expectedClasses = Set("A", "B")
    assert(declaredClasses === expectedClasses)
  }

  it should "handle non default package" in {
    val src = """
			|package a
			|class A
			|object B
			|""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val declaredClasses = compilerForTesting.extractDeclaredClassesFromSrc(src)
    val expectedClasses = Set("a.A", "a.B")
    assert(declaredClasses === expectedClasses)
  }

  it should "extract nested classes" in {
    val src = """
			|class A { class AA; object AAO }
			|object B { class BB; object BBO }
			|""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val declaredClasses = compilerForTesting.extractDeclaredClassesFromSrc(src)
    val expectedClasses = Set("A", "A.AA", "A.AAO", "B", "B.BB", "B.BBO")
    assert(declaredClasses === expectedClasses)
  }

  it should "extract private class" in {
    val src = """
			|class A { private class AA; private[A] class BB }
			|""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val declaredClasses = compilerForTesting.extractDeclaredClassesFromSrc(src)
    val expectedClasses = Set("A", "A.BB")
    assert(declaredClasses === expectedClasses)
  }

  it should "not extract class in a def" in {
    val src = """
			|class A {
			|  def foo = { class B }
			|}
			|""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val declaredClasses = compilerForTesting.extractDeclaredClassesFromSrc(src)
    val expectedClasses = Set("A")
    assert(declaredClasses === expectedClasses)
  }

  it should "handle companions" in {
    val src = """
			|class A; object A
			|""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val declaredClasses = compilerForTesting.extractDeclaredClassesFromSrc(src)
    val expectedClasses = Set("A")
    assert(declaredClasses === expectedClasses)
  }

  it should "extract traits" in {
    val src = """
			|trait A {
			|  class B
			|  object C
			|}
			|""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val declaredClasses = compilerForTesting.extractDeclaredClassesFromSrc(src)
    val expectedClasses = Set("A", "A.B", "A.C")
    assert(declaredClasses === expectedClasses)
  }

}
