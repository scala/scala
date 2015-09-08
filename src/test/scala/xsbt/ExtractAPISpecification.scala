package xsbt

import org.junit.runner.RunWith
import xsbti.api.ClassLike
import xsbti.api.Def
import xsbt.api.SameAPI
import sbt.internal.util.UnitSpec

class ExtractAPISpecification extends UnitSpec {

  "Existential types in method signatures" should "have stable names" in stableExistentialNames()

  def stableExistentialNames() = {
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
    assert(SameAPI.apply(fooMethodApi1, fooMethodApi2), "APIs are not the same.")
  }
}
