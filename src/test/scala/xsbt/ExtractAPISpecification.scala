package xsbt

import org.junit.runner.RunWith
import xsbti.api.ClassLike
import xsbti.api.Def
import xsbt.api.SameAPI
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExtractAPISpecification extends Specification {

  "Existential types in method signatures" should {
    "have stable names" in { stableExistentialNames }
  }

  "Children of a sealed class" in {
    def compileAndGetFooClassApi(src: String): ClassLike = {
      val compilerForTesting = new ScalaCompilerForUnitTesting
      val sourceApi = compilerForTesting.extractApiFromSrc(src)
      val FooApi = sourceApi.definitions().find(_.name() == "Foo").get.asInstanceOf[ClassLike]
      FooApi
    }
    val src1 =
      """|sealed abstract class Foo
         |case class C1(x: Int) extends Foo
         |""".stripMargin
    val fooClassApi1 = compileAndGetFooClassApi(src1)
    val src2 =
      """|sealed abstract class Foo
         |case class C1(x: Int) extends Foo
         |case class C2(x: Int) extends Foo
         |""".stripMargin
    val fooClassApi2 = compileAndGetFooClassApi(src2)
    SameAPI(fooClassApi1, fooClassApi2) !=== true
  }

  def stableExistentialNames: Boolean = {
    def compileAndGetFooMethodApi(src: String): Def = {
      val compilerForTesting = new ScalaCompilerForUnitTesting
      val sourceApi = compilerForTesting.extractApiFromSrc(src)
      val FooApi = sourceApi.definitions().find(_.name() == "Foo").get.asInstanceOf[ClassLike]
      val fooMethodApi = FooApi.structure().declared().find(_.name == "foo").get
      fooMethodApi.asInstanceOf[Def]
    }
    val src1 = """
				|class Box[T]
				|class Foo {
				|	def foo: Box[_] = null
				|
				}""".stripMargin
    val fooMethodApi1 = compileAndGetFooMethodApi(src1)
    val src2 = """
				|class Box[T]
				|class Foo {
			    |   def bar: Box[_] = null
				|	def foo: Box[_] = null
				|
				}""".stripMargin
    val fooMethodApi2 = compileAndGetFooMethodApi(src2)
    SameAPI.apply(fooMethodApi1, fooMethodApi2)
  }
}
