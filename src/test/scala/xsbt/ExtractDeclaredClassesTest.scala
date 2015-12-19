package xsbt

import org.junit.runner.RunWith
import xsbti.api.ClassLike
import xsbti.api.Def
import xsbti.api.Package
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert._

import org.junit.runner.RunWith
import xsbti.api._
import xsbt.api.HashAPI
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExtractDeclaredClassesTest extends Specification {

  "default package" in {
    val src = """
			|class A
			|object B
			|""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val declaredClasses = compilerForTesting.extractDeclaredClassesFromSrc(src)
    val expectedClasses = Set("A", "B$")
    declaredClasses === expectedClasses
  }

  "non default package" in {
    val src = """
			|package a
			|class A
			|object B
			|""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val declaredClasses = compilerForTesting.extractDeclaredClassesFromSrc(src)
    val expectedClasses = Set("a.A", "a.B$")
    declaredClasses === expectedClasses
  }

  "nested" in {
    val src = """
			|class A { class AA; object AAO }
			|object B { class BB; object BBO }
			|""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val declaredClasses = compilerForTesting.extractDeclaredClassesFromSrc(src)
    val expectedClasses = Set("A", "A.AA", "A.AAO$", "B$", "B$.BB", "B$.BBO$")
    declaredClasses === expectedClasses
  }

  "private class" in {
    val src = """
			|class A { private class AA; private[A] class BB }
			|""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val declaredClasses = compilerForTesting.extractDeclaredClassesFromSrc(src)
    val expectedClasses = Set("A", "A.BB")
    declaredClasses === expectedClasses
  }

  "class in def" in {
    val src = """
			|class A {
			|  def foo = { class B }
			|}
			|""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val declaredClasses = compilerForTesting.extractDeclaredClassesFromSrc(src)
    val expectedClasses = Set("A")
    declaredClasses === expectedClasses
  }

  "companions" in {
    val src = """
			|class A; object A
			|""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val declaredClasses = compilerForTesting.extractDeclaredClassesFromSrc(src)
    val expectedClasses = Set("A", "A$")
    declaredClasses === expectedClasses
  }

  "traits" in {
    val src = """
			|trait A {
			|  class B
			|  object C
			|}
			|""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val declaredClasses = compilerForTesting.extractDeclaredClassesFromSrc(src)
    val expectedClasses = Set("A", "A.B", "A.C$")
    declaredClasses === expectedClasses
  }

}
