import scala.tools.partest._

object Test
extends DirectTest
{
  override def extraSettings: String = "-usejavacp -Vimplicits -Vtype-diffs"

  def code: String = ""

  def chain: String = """
object ImplicitChain
{
  trait I1
  trait I2
  trait I3
  trait I4
  trait II
  implicit def i1(implicit impPar7: I3): I1 = ???
  implicit def i2a(implicit impPar8: I3): I2 = ???
  implicit def i2b(implicit impPar8: I3): I2 = ???
  implicit def i4(implicit impPar9: I2): I4 = ???
  implicit def g(implicit impPar3: I1, impPar1: I4): II = ???
  implicitly[II]
}
  """

  def foundReq: String = """
object FoundReq
{
  class L
  type R
  def f(r: R): Int = ???
  f(new L)
}
  """

  def bounds: String = """
object Bounds
{
  trait Base
  trait Arg
  trait F[A]
  implicit def g[A <: Base, B]: F[A] = ???
  implicitly[F[Arg]]
}
  """

  def longAnnotationMessage: String = """
object Long
{
  def long(implicit ec: concurrent.ExecutionContext): Unit = ???
  long
}
  """

  def longInfix: String = """
object InfixBreak
{
  type ::::[A, B]
  trait VeryLongTypeName
  trait Short
  type T1 = VeryLongTypeName :::: VeryLongTypeName :::: VeryLongTypeName ::::
    VeryLongTypeName
  type T2 = T1 :::: (Short :::: Short) :::: T1 :::: T1
  implicit def f(implicit impPar4: List[T2]): String = ???
  implicitly[String]
}
  """

  def deeplyNestedHole: String = """
object DeepHole
{
  trait C1[F[_]]
  trait C2[F[_], G[_], A]
  trait C3[A, B]
  trait C4[A]
  type Id[A] = A
  type T1[X] = C3[List[String], X]
  type T2[Y] = C2[Id, C4, Y]
  type T3[Z] = C2[T1, T2, Z]
  implicitly[C1[T3]]
}
  """

  def auxType: String = """
object Aux
{
  trait C
  trait D
  trait F
  object F { type Aux[A, B] = F { type X = A; type Y = B } }
  implicit def f[A, B](implicit impPar10: C): F { type X = A; type Y = B } =
    ???
  implicitly[F.Aux[C, D]]
}
  """

  def refined: String = """
object Refined
{
  trait A
  trait B
  trait C
  trait D
  trait E
  trait F
  def f(a: A with B with C { type Y = String; type X = String; type Z = String }): Unit = ???
  val x: B with E with A with F { type X = Int; type Y = String } = ???
  f(x)
}
  """

  def disambiguateQualified: String = """
object A
{
  object B
  {
    object X
    {
      object Y
      {
        type T
      }
    }
  }
  object C
  {
    object X
    {
      object Y
      {
        type T
      }
    }
  }
  def f(a: B.X.Y.T): Unit = ()
  val x: C.X.Y.T = ???
  f(x: C.X.Y.T)
}
  """

  def bynameParam: String = """
object Foo
{
  type A
  type B
  def f(g: (=> A) => B): Unit = ()
  f(1: Int)
}
  """

  def tuple1: String = """
object Tup1
{
  val a: Tuple1[String] = "Tuple1": String
}
  """

  def singleType: String = """
object SingleImp
{
  class ***[A, B]
  val a = 1
  val b = 2

  implicitly[a.type *** b.type]
}
  """

  def singleTypeInFunction: String = """
object SingleImp
{
  class ***[A, B]
  def fn(): Unit = {
    val a = 1
    val b = 2

    implicitly[a.type *** b.type]
  }
}
  """

  def singleTypeWithFreeSymbol: String = """
object SingleImp
{
  class ***[A, B]
  def fn[A, B](a: A, b: B) = {

    implicitly[a.type *** b.type]
  }
}
  """

  def parameterAnnotation: String = """
  import collection.{mutable => m, immutable => i}
  object Test {
    val o = new Object
    val ms = m.SortedSet(1,2,3)
    ms.map(_ => o)
  }
  """

  def show(): Unit = {
    val global = newCompiler()

    def run(code: String): Unit =
      compileString(global)(code.trim)

    run(chain)
    run(foundReq)
    run(bounds)
    run(longAnnotationMessage)
    run(longInfix)
    run(deeplyNestedHole)
    run(auxType)
    run(refined)
    run(disambiguateQualified)
    run(bynameParam)
    run(tuple1)
    run(singleType)
    run(singleTypeInFunction)
    run(singleTypeWithFreeSymbol)
    run(parameterAnnotation)
  }
}
