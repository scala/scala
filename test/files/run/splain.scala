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


  def show() {
    val global = newCompiler()
    import global._
    import analyzer._

    object analyzerPlugin extends AnalyzerPlugin {
      override def noImplicitFoundError(param: Symbol, errors: List[ImpFailReason]): Option[String] = {
        val chain = errors.map(_.candidateName).mkString(", ")
        Some(s"no implicit for $param; chains: $chain")
      }
    }

    addAnalyzerPlugin(analyzerPlugin)
    compileString(global)(code)
  }
}
