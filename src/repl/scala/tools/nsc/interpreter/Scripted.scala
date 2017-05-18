/* NSC -- new Scala compiler
 * Copyright 2005-2017 LAMP/EPFL and Lightbend, Inc.
 */
package scala.tools.nsc.interpreter

import java.io.PrintWriter

import scala.reflect.internal.util.Position
import scala.tools.nsc.Settings

object ImportContextPreamble {
  def empty = ImportContextPreamble(Set.empty, Set.empty, "")
}
case class ImportContextPreamble(exclude: Set[String], include: Set[String], preamble: String)

// TODO: `importContextPreamble` breaks the separation between the repl's core and the frontend
// because it's a callback in the wrong direction (the frontend is only supposed to call us, we shouldn't know about the frontend)
class ScriptedInterpreter(initialSettings: Settings, reporter: ReplReporter, importContextPreamble: Set[String] => ImportContextPreamble) extends IMain(initialSettings, None, initialSettings, reporter) {

  import global.{Name, TermName}

  /* Modify the template to snag definitions from dynamic context.
   * So object $iw { x + 42 } becomes object $iw { def x = $ctx.x ; x + 42 }
   */
  override protected def importsCode(wanted: Set[Name], wrapper: Request#Wrapper, definesClass: Boolean, generousImports: Boolean) = {
    val ImportContextPreamble(exclude, include, sciptedPreamble) =
      importContextPreamble(wanted.filter(_.isTermName).map(_.decodedName.toString))

    if (exclude.nonEmpty) {
      val scriptedWanted = (wanted &~ exclude.map(TermName.apply)) ++ include.map(TermName.apply)

      val ComputedImports(header, preamble, trailer, path) =
        super.importsCode(scriptedWanted, wrapper, definesClass, generousImports)

      ComputedImports(header, preamble + sciptedPreamble, trailer, path)
    }
    else super.importsCode(wanted, wrapper, definesClass, generousImports)
  }

}
