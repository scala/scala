package xsbt

import xsbti.api._
import xsbt.api.SameAPI
import sbt.internal.util.UnitSpec

class ExtractAPISpecification extends UnitSpec {

  "ExtractAPI" should "give stable names to members of existential types in method signatures" in stableExistentialNames()

  it should "extract children of a sealed class" in {
    def compileAndGetFooClassApi(src: String): ClassLike = {
      val compilerForTesting = new ScalaCompilerForUnitTesting
      val apis = compilerForTesting.extractApisFromSrc(src)
      val FooApi = apis.find(_.name() == "Foo").get
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
    assert(SameAPI(fooClassApi1, fooClassApi2) !== true)
  }

  it should "extract correctly the definition type of a package object" in {
    val src = "package object foo".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val apis = compilerForTesting.extractApisFromSrc(src)
    val Seq(fooClassApi) = apis.toSeq
    assert(fooClassApi.definitionType === DefinitionType.PackageModule)
  }

  it should "extract nested classes" in {
    val src =
      """class A {
        |  class B
        |}""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val apis = compilerForTesting.extractApisFromSrc(src).map(c => c.name -> c).toMap
    assert(apis.keys === Set("A", "A.B"))
  }

  it should "not extract local classes" in {
    val src =
      """class A
        |class B
        |class C { def foo: Unit = { class Inner2 extends B } }
        |class D { def foo: Unit = { new B {} } }""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val apis = compilerForTesting.extractApisFromSrc(src).map(c => c.name -> c).toMap
    assert(apis.keys === Set("A", "B", "C", "D"))
  }

  it should "extract flat (without members) api for a nested class" in {
    def compileAndGetFooClassApi(src: String): ClassLike = {
      val compilerForTesting = new ScalaCompilerForUnitTesting
      val apis = compilerForTesting.extractApisFromSrc(src)
      val FooApi = apis.find(_.name() == "Foo").get
      FooApi
    }
    val src1 =
      """class Foo {
        |  class A
        |}""".stripMargin
    val fooClassApi1 = compileAndGetFooClassApi(src1)
    val src2 =
      """class Foo {
        |  class A {
        |    def foo: Int = 123
        |  }
        |}""".stripMargin
    val fooClassApi2 = compileAndGetFooClassApi(src2)
    assert(SameAPI(fooClassApi1, fooClassApi2) === true)
  }

  it should "extract private classes" in {
    val src =
      """private class A
        |class B { private class Inner1 extends A }
        |""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val apis = compilerForTesting.extractApisFromSrc(src).map(c => c.name -> c).toMap
    assert(apis.keys === Set("A", "B", "B.Inner1"))
  }

  def stableExistentialNames(): Unit = {
    def compileAndGetFooMethodApi(src: String): Def = {
      val compilerForTesting = new ScalaCompilerForUnitTesting
      val sourceApi = compilerForTesting.extractApisFromSrc(src)
      val FooApi = sourceApi.find(_.name() == "Foo").get
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

  /**
   * Checks if representation of the inherited Namer class (with a declared self variable) in Global.Foo
   * is stable between compiling from source and unpickling. We compare extracted APIs of Global when Global
   * is compiled together with Namers or Namers is compiled first and then Global refers
   * to Namers by unpickling types from class files.
   */
  it should "make a stable representation of a self variable that has no self type" in {
    def selectNamer(apis: Set[ClassLike]): ClassLike = {
      // TODO: this doesn't work yet because inherited classes are not extracted
      apis.find(_.name == "Global.Foo.Namer").get
    }
    val src1 =
      """|class Namers {
        |  class Namer { thisNamer => }
        |}
        |""".stripMargin
    val src2 =
      """|class Global {
        |  class Foo extends Namers
        |}
        |""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val apis = compilerForTesting.extractApisFromSrcs(reuseCompilerInstance = false)(List(src1, src2), List(src2))
    val _ :: src2Api1 :: src2Api2 :: Nil = apis.toList
    val namerApi1 = selectNamer(src2Api1)
    val namerApi2 = selectNamer(src2Api2)
    assert(SameAPI(namerApi1, namerApi2))
  }

  it should "make a different representation for an inherited class" in {
    val src =
      """|class A[T] {
         |  abstract class AA { def t: T }
         |}
         |class B extends A[Int]
      """.stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val apis = compilerForTesting.extractApisFromSrc(src).map(a => a.name -> a).toMap
    assert(apis.keySet === Set("A", "A.AA", "B", "B.AA"))
    assert(apis("A.AA") !== apis("B.AA"))
  }

  it should "handle package objects and type companions" in {
    val src =
      """|package object abc {
         |  type BuildInfoKey = BuildInfoKey.Entry[_]
         |  object BuildInfoKey {
         |    sealed trait Entry[A]
         |  }
         |}
      """.stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val apis = compilerForTesting.extractApisFromSrc(src).map(a => a.name -> a).toMap
    assert(apis.keySet === Set("abc.package", "abc.BuildInfoKey", "abc.BuildInfoKey.Entry"))
  }

  /**
   * Checks if self type is properly extracted in various cases of declaring a self type
   * with our without a self variable.
   */
  it should "represent a self type correctly" in {
    val srcX = "trait X"
    val srcY = "trait Y"
    val srcC1 = "class C1 { this: C1 => }"
    val srcC2 = "class C2 { thisC: C2 => }"
    val srcC3 = "class C3 { this: X => }"
    val srcC4 = "class C4 { thisC: X => }"
    val srcC5 = "class C5 extends AnyRef with X with Y { self: X with Y => }"
    val srcC6 = "class C6 extends AnyRef with X { self: X with Y => }"
    val srcC7 = "class C7 { _ => }"
    val srcC8 = "class C8 { self => }"
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val apis = compilerForTesting.extractApisFromSrcs(reuseCompilerInstance = true)(
      List(srcX, srcY, srcC1, srcC2, srcC3, srcC4, srcC5, srcC6, srcC7, srcC8)
    ).map(_.head)
    val emptyType = new EmptyType
    def hasSelfType(c: ClassLike): Boolean =
      c.selfType != emptyType
    val (withSelfType, withoutSelfType) = apis.partition(hasSelfType)
    assert(withSelfType.map(_.name).toSet === Set("C3", "C4", "C5", "C6"))
    assert(withoutSelfType.map(_.name).toSet === Set("X", "Y", "C1", "C2", "C7", "C8"))
  }
}
