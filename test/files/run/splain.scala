import scala.tools.partest._

object Test
extends DirectTest
{
  override def extraSettings: String = "-usejavacp"

  def code = """
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
  """.trim

  def foundReq = """
object FoundReq
{
  class L
  type R
  def f(r: R) = ???
  f(new L)
}
  """.trim

  def bounds = """
object Bounds
{
  trait Base
  trait Arg
  trait F[A]
  implicit def g[A <: Base, B]: F[A] = ???
  implicitly[F[Arg]]
}
  """.trim


  def show() {
    val global = newCompiler()
    import global._
    import analyzer._

    object analyzerPlugin extends AnalyzerPlugin {
      override def noImplicitFoundError(param: Symbol, errors: List[ImplicitError]): Option[String] = {
        val chain = errors
          .map {
            case a @ ImplicitError.NotFound(_, _, _, _) => a.candidateName
            case b @ ImplicitError.NonconformantBounds(_, _, _, a, p, _) =>
              val diff = a.zip(p).map { case (l, r) => s"$l/$r" }.mkString("[", ", ", "]")
              s"${b.candidateName}: $diff"
          }
          .mkString(", ")
        Some(s"no implicit for $param; chains: $chain")
      }

      override def foundReqMsg(found: Type, req: Type): Option[String] = {
        Some(s"\nfound: $found, req: $req")
      }
    }

    addAnalyzerPlugin(analyzerPlugin)
    compileString(global)(code)
    compileString(global)(foundReq)
    compileString(global)(bounds)
  }
}
