package xsbt

import org.junit.runner.RunWith
import xsbti.TestCallback.ExtractedClassDependencies
import xsbti.api.ClassLike
import xsbti.api.Def
import xsbt.api.SameAPI
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ClassNameSpecification extends Specification {

  "Binary names for top level object" in {
    val src = "object A"

    val compilerForTesting = new ScalaCompilerForUnitTesting(nameHashing = true)
    val binaryClassNames = compilerForTesting.extractBinaryClassNamesFromSrc(src)

    binaryClassNames === Set("A" -> "A", "A" -> "A$")
  }

  "Binary names for top level companion object" in {
    val src = "class A; object A"

    val compilerForTesting = new ScalaCompilerForUnitTesting(nameHashing = true)
    val binaryClassNames = compilerForTesting.extractBinaryClassNamesFromSrc(src)

    binaryClassNames === Set("A" -> "A", "A" -> "A$")
  }

  "Binary names for nested object" in {
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

    val compilerForTesting = new ScalaCompilerForUnitTesting(nameHashing = true)
    val binaryClassNames = compilerForTesting.extractBinaryClassNamesFromSrc(src)

    binaryClassNames === Set("A" -> "A$", "A" -> "A", "A.C" -> "A$C$", "A.C.D" -> "A$C$D$",
      "B" -> "B", "B.E" -> "B$E$")
  }

  "Binary names for trait" in {
    val src =
      """|trait A
      """.stripMargin

    val compilerForTesting = new ScalaCompilerForUnitTesting(nameHashing = true)
    val binaryClassNames = compilerForTesting.extractBinaryClassNamesFromSrc(src)

    // we do not track $impl classes because nobody can depend on them directly
    binaryClassNames === Set("A" -> "A")
  }

}
