/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.cmd
package gen

class Codegen(args: List[String]) extends {
  val parsed = CodegenSpec(args: _*)
} with CodegenSpec with Instance

object Codegen {
  def echo(msg: String) = Console println msg

  def main(args0: Array[String]): Unit = {
    val runner = new Codegen(args0.toList)
    import runner._

    if (args0.isEmpty)
      return println (CodegenSpec.helpMsg)

    val out = outDir getOrElse { return println("--out is required.") }
    val all = genall || !anyvals

    echo("Generating sources into " + out)

    if (anyvals || all) {
      val av = new AnyVals { }

      av.make() foreach { case (name, code ) =>
        val file = (out / (name + ".scala")).toFile
        echo("Writing: " + file)
        file writeAll code
      }
    }
  }
}

