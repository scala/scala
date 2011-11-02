import scala.tools.nsc.interpreter._

object Test {
  def run(args: String) = {
    println("Arguments: '" + args + "'")
    ILoop.run("""
      |class Bippy {
      |  private def privateMethod = 5
      |  def f[T <: List[_]](x: T): T = x
      |}
      |
      |:javap %s Bippy
    """.stripMargin.format(args)).lines map (_.trim) filter { line =>
      (line startsWith "private") || (line startsWith "public")
    } foreach println
  }
  
  def main(args: Array[String]): Unit = {
    run("")
    run("-v")
    run("-s")
    run("-private")
  }
}
