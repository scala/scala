/* NEST (New Scala Test)
 * Copyright 2007-2012 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.cmd
package gen

import FromString.ExistingDir

trait CodegenSpec extends Spec with Meta.StdOpts with Interpolation {
  def referenceSpec       = CodegenSpec
  def programInfo         = Spec.Info("codegen", "", "scala.tools.cmd.gen.Codegen")

  import FromString.ExistingDir

  help("Usage: codegen [<options>]")

  // val inDir    = "in" / "directory containing templates" --^ ExistingDir
  val outDir   = "out" / "directory for generated files" --^ ExistingDir
  // val install  = "install" / "write source files directly to src/library/scala"
  val anyvals  = "anyvals" / "generate sources for AnyVal types" --?
  val products = "products" / "generate sources for ProductN, FunctionN, etc." --?
  val genall   = "all" / "generate sources for everything" --?
  val stamp    = "stamp" / "add a timestamp to the generated files" --?
}

object CodegenSpec extends CodegenSpec with Reference {
  type ThisCommandLine = CommandLine
  def creator(args: List[String]): ThisCommandLine = new CommandLine(CodegenSpec, args)
}
