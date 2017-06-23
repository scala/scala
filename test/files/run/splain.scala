import scala.tools.partest._
import scala.tools.nsc._

object Test
extends DirectTest
{
  override def extraSettings: String = "-usejavacp"

  def code = """
object types
{
  class ***[A, B]
  class >:<[A, B]
  class C
  trait D
}
import types._

trait Low
{
  trait I1
  trait I2
  trait I3
  trait I4
  trait F[X[_]]
  implicit def lowI1: I1 = ???
  implicit def lowI2: I2 = ???
}

object ImplicitChain
extends Low
{
  type T1 = C *** D >:< (C with D { type A = D; type B = C })
  type T2 = D *** ((C >:< C) *** (D => Unit))
  type T3 = (D *** (C *** String)) >:< ((C, D, C) *** D)
  type T4 = C *** D *** C
  type T5 = D *** C >:< D
  type T7 = D >:< C >:< D
  implicit def i1(implicit impPar7: I3): I1 = ???
  implicit def i2a(implicit impPar8: I3): I2 = ???
  implicit def i2b(implicit impPar8: I3): I2 = ???
  implicit def i4(implicit impPar9: I2): I4 = ???
  implicit def t7(implicit impPar14: F[({type λ[X] = Either[Int, X]})#λ]): T7 = ???
  implicit def t4(implicit impPar12: T5): T4 = ???
  implicit def t3a(implicit impPar11: T7): T3 = ???
  implicit def t3b(implicit impPar10: T4): T3 = ???
  implicit def f(implicit impPar4: I4, impPar2: T3): T2 = ???
  implicit def g(implicit impPar3: I1, impPar1: T2): T1 = ???
  implicitly[T1]
}
  """.trim


  def show() {
    val global = newCompiler()
    import global._
    import analyzer._

    object analyzerPlugin extends AnalyzerPlugin {
      override def noImplicitFoundError(tree: Tree, param: Symbol): Option[String] = {
        Some(s"no implicit for $param")
      }

    }

    addAnalyzerPlugin(analyzerPlugin)
    compileString(global)(code)
  }

}
