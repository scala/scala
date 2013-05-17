/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.cmd
package gen

import FromString.ExistingDir

trait CodegenSpec extends Spec with Meta.StdOpts with Interpolation {
  def referenceSpec       = CodegenSpec
  def programInfo         = Spec.Info("codegen", "", "scala.tools.cmd.gen.Codegen")

  help("Usage: codegen [<options>]")

  val outDir   = "out" / "directory for generated files" --^ ExistingDir
  val anyvals  = "anyvals" / "generate sources for AnyVal types" --?
  val genall   = "all" / "generate sources for everything" --?
}

object CodegenSpec extends CodegenSpec with Reference {
  type ThisCommandLine = CommandLine
  def creator(args: List[String]): ThisCommandLine = new CommandLine(CodegenSpec, args)
}
