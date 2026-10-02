package scala.tools.xsbt

import org.junit.Test
import org.junit.Assert._
import xsbti.api._

class ExtractAPITest extends BridgeTesting {

  @Test
  def `ExtractAPI should extract children of a sealed class`(): Unit = {
    def compileAndGetFooClassApi(src: String): ClassLike = {
      val apis = extractApisFromSrc(src)
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
    assertFalse(SameAPI(fooClassApi1, fooClassApi2))
  }

  @Test
  def `ExtractAPI should extract correctly the definition type of a package object`(): Unit = {
    val src = "package object foo".stripMargin
    val apis = extractApisFromSrc(src)
    val Seq(fooClassApi) = apis.toSeq
    assertEquals(fooClassApi.definitionType, DefinitionType.PackageModule)
  }

  // Default getters of constructor arguments are all named `<init>$default$N`, so a change to
  // one class's constructor would invalidate clients of every other class with one.
  @Test
  def `ExtractAPI should name default getters of constructor arguments after their class`(): Unit = {
    val src =
      """|class B(z: Int) { def this(x: Int, y: Int = 2) = this(x + y) }
         |class C(z: String) { def this(x: Int, y: Int = 2) = this("") }
         |""".stripMargin
    val apis = extractApisFromSrc(src)
    def moduleDefNames(name: String): Set[String] = {
      val module = apis.find(c => c.name == name && c.definitionType == DefinitionType.Module).get
      module.structure.declared.map(_.name).toSet
    }
    assertEquals(Set("B;init;default;2"), moduleDefNames("B").filter(_.contains("default")))
    assertEquals(Set("C;init;default;2"), moduleDefNames("C").filter(_.contains("default")))
  }

  // Regression guard for https://github.com/sbt/sbt/issues/1079.
  // A type parameter of a type alias declared inside a refinement (a type lambda) is named
  // by `Symbol.fullName`, whose owner chain passes through the anonymous `<refinement>`
  // class. The pickler rewrites that class's owner (scala/bug#6596), so the parameter is
  // named `test.<refinement>.a` from source and `test.KleisliMonadReader.<refinement>.a`
  // when unpickled, flipping the API hash of every class built on the type lambda.
  @Test
  def `ExtractAPI should give stable names to type parameters owned by a refinement class (sbt-sbt-1079)`(): Unit = {
    val monadReader =
      """|package test
         |trait MonadReader[F[_], R] {
         |  def ask: F[R]
         |  def local[A](f: R => R)(fa: F[A]): F[A]
         |}
         |""".stripMargin
    val kleisli =
      """|package test
         |case class Kleisli[F[_], R, A](run: R => F[A])
         |trait KleisliMonadReader[F[_], R]
         |    extends MonadReader[({ type l[a] = Kleisli[F, R, a] })#l, R] {
         |  def ask: Kleisli[F, R, R] = ???
         |  def local[A](f: R => R)(fa: Kleisli[F, R, A]): Kleisli[F, R, A] = ???
         |}
         |""".stripMargin
    val impl =
      """|package test
         |class Impl extends KleisliMonadReader[Option, Int]
         |""".stripMargin
    // compile everything together (from source), then recompile `impl` alone,
    // unpickling the support types from class files.
    val apis = extractApisFromSrcs(List(monadReader, kleisli, impl), List(impl))
    val List(_, _, implFromSource, implUnpickled) = apis.toList
    def implClass(as: Set[ClassLike]): ClassLike = as.find(_.name == "test.Impl").get
    val fromSource = implClass(implFromSource)
    val unpickled = implClass(implUnpickled)
    // Upstream additionally asserts on the rendered API (ShowAPI, not available in this test suite)
    // that the parameter is named `<refinement>.a` with no owner prefix.
    assertTrue(
      "Impl API differs between compiling from source and unpickling",
      SameAPI(fromSource, unpickled)
    )
  }

  // Since sbt/zinc#1782 a type parameter declared in a refinement is named relative to the
  // outermost refinement, so the parameters of sibling type lambdas share a name.
  @Test
  def `ExtractAPI should tell apart type parameters of sibling refinements with the same name`(): Unit = {
    def fooApi(arg1: String, arg2: String): ClassLike = {
      val src =
        s"""|trait Two[F[_], G[_]]
            |class X[A]
            |class Y[A]
            |class H[F[_]]
            |trait Foo extends Two[({ type l[a] = $arg1 })#l, ({ type l[a] = $arg2 })#l]
            |""".stripMargin
      extractApisFromSrc(src).find(_.name == "Foo").get
    }
    def nested(body: String): String = s"H[({ type m[b] = $body })#m]"
    assertTrue(SameAPI(fooApi("X[a]", "Y[a]"), fooApi("X[a]", "Y[a]")))
    assertFalse(SameAPI(fooApi("X[a]", "Y[a]"), fooApi("Y[a]", "X[a]")))
    assertFalse(SameAPI(fooApi("X[a]", "X[Int]"), fooApi("X[Int]", "X[a]")))
    assertTrue(SameAPI(fooApi(nested("Map[a, b]"), "X[a]"), fooApi(nested("Map[a, b]"), "X[a]")))
    assertFalse(SameAPI(fooApi(nested("Map[a, b]"), "X[a]"), fooApi(nested("Map[b, a]"), "X[a]")))
    assertFalse(
      SameAPI(
        fooApi(nested("Map[a, b]"), nested("Map[b, a]")),
        fooApi(nested("Map[b, a]"), nested("Map[a, b]"))
      )
    )
  }

  @Test
  def `ExtractAPI should extract nested classes`(): Unit = {
    val src =
      """class A {
        |  class B
        |}""".stripMargin
    val apis = extractApisFromSrc(src).map(c => c.name -> c).toMap
    assertEquals(apis.keys, Set("A", "A.B"))
  }

  @Test
  def `ExtractAPI should not extract local classes`(): Unit = {
    val src =
      """class A
        |class B
        |class C { def foo: Unit = { class Inner2 extends B } }
        |class D { def foo: Unit = { new B {} } }""".stripMargin
    val apis = extractApisFromSrc(src).map(c => c.name -> c).toMap
    assertEquals(apis.keys, Set("A", "B", "C", "D"))
  }

  @Test
  def `ExtractAPI should extract flat (without members) api for a nested class`(): Unit = {
    def compileAndGetFooClassApi(src: String): ClassLike = {
      val apis = extractApisFromSrc(src)
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
    assertTrue(SameAPI(fooClassApi1, fooClassApi2))
  }

  @Test
  def `ExtractAPI should extract private classes`(): Unit = {
    val src =
      """private class A
        |class B { private class Inner1 extends A }
        |""".stripMargin
    val apis = extractApisFromSrc(src).map(c => c.name -> c).toMap
    assertEquals(apis.keys, Set("A", "B", "B.Inner1"))
  }

  @Test
  def `ExtractAPI should give stable names to members of existential types in method signatures`(): Unit = {
    def compileAndGetFooMethodApi(src: String): Def = {
      val sourceApi = extractApisFromSrc(src)
      val FooApi = sourceApi.find(_.name() == "Foo").get
      val fooMethodApi = FooApi.structure().declared().find(_.name == "foo").get
      fooMethodApi.asInstanceOf[Def]
    }

    val src1 =
      """class Box[T]
        |class Foo {
        |	 def foo: Box[_] = null
        |}""".stripMargin
    val fooMethodApi1 = compileAndGetFooMethodApi(src1)
    val src2 =
      """class Box[T]
        |class Foo {
        |  def bar: Box[_] = null
        |	 def foo: Box[_] = null
        |}""".stripMargin
    val fooMethodApi2 = compileAndGetFooMethodApi(src2)
    assertTrue("APIs are not the same.", SameAPI.apply(fooMethodApi1, fooMethodApi2))
  }

  /**
   * Checks if representation of the inherited Namer class (with a declared self variable) in Global.Foo
   * is stable between compiling from source and unpickling. We compare extracted APIs of Global when Global
   * is compiled together with Namers or Namers is compiled first and then Global refers
   * to Namers by unpickling types from class files.
   */
  @Test
  def `ExtractAPI should make a stable representation of a self variable that has no self type`(): Unit = {
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
    val apis =
      extractApisFromSrcs(List(src1, src2), List(src2))
    val _ :: src2Api1 :: src2Api2 :: Nil = apis.toList
    val namerApi1 = selectNamer(src2Api1)
    val namerApi2 = selectNamer(src2Api2)
    assertTrue(SameAPI(namerApi1, namerApi2))
  }

  @Test
  def `ExtractAPI should make a different representation for an inherited class`(): Unit = {
    val src =
      """|class A[T] {
         |  abstract class AA { def t: T }
         |}
         |class B extends A[Int]
    """.stripMargin
    val apis = extractApisFromSrc(src).map(a => a.name -> a).toMap
    assertEquals(apis.keySet, Set("A", "A.AA", "B", "B.AA"))
    assertNotEquals(apis("A.AA"), apis("B.AA"))
  }

  @Test
  def `ExtractAPI should handle package objects and type companions`(): Unit = {
    val src =
      """|package object abc {
         |  type BuildInfoKey = BuildInfoKey.Entry[_]
         |  object BuildInfoKey {
         |    sealed trait Entry[A]
         |  }
         |}
    """.stripMargin
    val apis = extractApisFromSrc(src).map(a => a.name -> a).toMap
    assertEquals(apis.keySet, Set("abc.package", "abc.BuildInfoKey", "abc.BuildInfoKey.Entry"))
  }

  /**
   * Checks if self type is properly extracted in various cases of declaring a self type
   * with our without a self variable.
   */
  @Test
  def `ExtractAPI should represent a self type correctly`(): Unit = {
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
    val apis = extractApisFromSrcs(
      List(srcX, srcY, srcC1, srcC2, srcC3, srcC4, srcC5, srcC6, srcC7, srcC8)
    ).map(_.head)
    val emptyType = EmptyType.of()

    def hasSelfType(c: ClassLike): Boolean =
      c.selfType != emptyType

    val (withSelfType, withoutSelfType) = apis.partition(hasSelfType)
    assertEquals(withSelfType.map(_.name).toSet, Set("C3", "C4", "C5", "C6"))
    assertEquals(withoutSelfType.map(_.name).toSet, Set("X", "Y", "C1", "C2", "C7", "C8"))
  }
}
