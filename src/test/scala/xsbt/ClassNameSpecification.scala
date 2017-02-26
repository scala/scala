package xsbt

import xsbti.api.ClassLike
import xsbti.api.Def
import xsbt.api.SameAPI

import sbt.internal.util.UnitSpec

class ClassNameSpecification extends UnitSpec {

  "ClassName" should "create correct binary names for top level object" in {
    val src = "object A"

    val compilerForTesting = new ScalaCompilerForUnitTesting
    val binaryClassNames = compilerForTesting.extractBinaryClassNamesFromSrc(src)

    assert(binaryClassNames === Set("A" -> "A", "A" -> "A$"))
  }

  it should "create binary names for top level companions" in {
    val src = "class A; object A"

    val compilerForTesting = new ScalaCompilerForUnitTesting
    val binaryClassNames = compilerForTesting.extractBinaryClassNamesFromSrc(src)

    assert(binaryClassNames === Set("A" -> "A", "A" -> "A$"))
  }

  it should "create correct binary names for nested object" in {
    val src =
      """|object A {
         |  object C {
         |    object D
         |  }
         |}
         |class B {
         |  object E
         |}
      """.stripMargin

    val compilerForTesting = new ScalaCompilerForUnitTesting
    val binaryClassNames = compilerForTesting.extractBinaryClassNamesFromSrc(src)

    assert(binaryClassNames === Set("A" -> "A$", "A" -> "A", "A.C" -> "A$C$", "A.C.D" -> "A$C$D$",
      "B" -> "B", "B.E" -> "B$E$"))
  }

  it should "create a binary name for a trait" in {
    val src =
      """|trait A
      """.stripMargin

    val compilerForTesting = new ScalaCompilerForUnitTesting
    val binaryClassNames = compilerForTesting.extractBinaryClassNamesFromSrc(src)

    // we do not track $impl classes because nobody can depend on them directly
    assert(binaryClassNames === Set("A" -> "A"))
  }

  it should "not create binary names for local classes" in {
    val src = """
      |class Container {
      |  def foo = {
      |    class C
      |  }
      |  def bar = {
      |    // anonymous class
      |    new T {}
      |  }
      |}
      |
      |trait T
      |""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val binaryClassNames = compilerForTesting.extractBinaryClassNamesFromSrc(src)
    assert(binaryClassNames === Set("Container" -> "Container", "T" -> "T"))
  }

}
