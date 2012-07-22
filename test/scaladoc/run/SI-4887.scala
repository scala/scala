import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def code = """
      package scala.test.scaladoc.existentials {
        import language.higherKinds
        import language.existentials

        class X[T, U, V]

        trait TEST {
          type T
          type U
          type A
          def foo1(x: X[T, U, _]) = 3
          def foo2(x: X[Z[_], U, z.type] forSome {type Z[_] <: { def z: String }; val z: Z[_ <: Int]}) = 4
          def foo3(x: X[Z, Z, V] forSome { type Z <: T; type V <: T }) = 6
        }
      }
  """

  // no need for special settings
  def scaladocSettings = "-feature"

  def testModel(rootPackage: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    val base = rootPackage._package("scala")._package("test")._package("scaladoc")._package("existentials")
    val TEST = base._trait("TEST")

    val foo1 = TEST._method("foo1")
    assert(foo1.valueParams(0)(0).resultType.name == "X[T, U, _]",
           foo1.valueParams(0)(0).resultType.name + " == X[T, U, _]")

    val foo2 = TEST._method("foo2")
    assert(foo2.valueParams(0)(0).resultType.name == "X[Z[_], U, _ <: [_]AnyRef { def z: String } with Singleton]",
           foo2.valueParams(0)(0).resultType.name + " == X[Z[_], U, _ <: [_]AnyRef { def z: String } with Singleton]")

    val foo3 = TEST._method("foo3")
    assert(foo3.valueParams(0)(0).resultType.name == "X[Z, Z, V] forSome {type Z <: T, type V <: T}",
           foo3.valueParams(0)(0).resultType.name + " == X[Z, Z, V] forSome {type Z <: T, type V <: T}")
  }
}