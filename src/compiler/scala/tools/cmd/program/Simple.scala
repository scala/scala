/* NEST (New Scala Test)
 * Copyright 2007-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package cmd
package program

import Spec.Info

/** A boilerplate reducer for commands with simple requirements.  For examples,
 *  see Scmp and Tokens in this package.
 */
object Simple {
  type CommandLineTransform = SimpleCommandLine => SimpleCommandLine

  abstract class SimpleSpec(val programInfo: Info) extends Spec with Meta.StdOpts with Interpolation

  trait SimpleInstance extends SimpleSpec with Instance {
    val parsed: CommandLine
  }

  class SimpleReference(
    programInfo: Info,
    unary: List[(String, String)] = Nil,
    binary: List[(String, String)] = Nil,
    postCreation: CommandLineTransform = null
  ) extends SimpleSpec(programInfo) with Reference {

    spec =>

    if (programInfo.usage != "") help(programInfo.usage)
    unary foreach   { case (option, help) => option / help --? }
    binary foreach  { case (option, help) => option / help --| }

    type ThisCommandLine = SimpleCommandLine

    def creator(args: List[String]) = new SimpleCommandLine(spec, args)
    def instance(args: Array[String]): SimpleInstance = instance(args.toList)
    def instance(args: List[String]): SimpleInstance =
      new {
        val parsed = spec(args: _*)
      } with SimpleSpec(programInfo) with SimpleInstance {
        lazy val referenceSpec = spec
      }

    lazy val referenceSpec = spec
  }

  def apply(info: Info, unary: List[(String, String)], binary: List[(String, String)], postCreation: CommandLineTransform): SimpleReference = {
    new SimpleReference(info, unary, binary, postCreation) {
      override def creator(args: List[String]) = {
        val obj = super.creator(args)
        if (postCreation == null) obj
        else postCreation(obj)
      }
    }
  }

  def scalaProgramInfo(name: String, help: String) =
    Spec.Info(name, help, "scala.tools.cmd.program." + name.capitalize)

  /** You can't override a def with a var unless a setter exists.  We cleverly
   *  sidestep this by mixing in a trait with dummy setters which will be
   *  inaccessible due to the overriding var.
   */
  trait Ticket2338WontFixWorkaround {
    def enforceArity_=(x: Boolean): Unit = sys.error("unreachable")
    def onlyKnownOptions_=(x: Boolean): Unit = sys.error("unreachable")
  }

  /** Configurability simplicity achieved by turning defs into vars and letting
   *  the spec creator apply a transformation.  This way there's no need to create
   *  custom subclasses of CommandLine.
   */
  class SimpleCommandLine(spec: Reference, args: List[String]) extends CommandLine(spec, args) with Ticket2338WontFixWorkaround {
    override var enforceArity: Boolean = true
    override var onlyKnownOptions: Boolean = true
  }
}
