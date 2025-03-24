/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.interpreter

import scala.tools.nsc.Settings

object ImportContextPreamble {
  def empty = ImportContextPreamble(Set.empty, Set.empty, "")
}
case class ImportContextPreamble(exclude: Set[String], include: Set[String], preamble: String)

// TODO: `importContextPreamble` breaks the separation between the repl's core and the frontend
// because it's a callback in the wrong direction (the frontend is only supposed to call us, we shouldn't know about the frontend)
class ScriptedInterpreter(initialSettings: Settings, reporter: ReplReporter, importContextPreamble: Set[String] => ImportContextPreamble)
  extends IMain(initialSettings, None, initialSettings, reporter) with ScriptedRepl {

  import global.{Name, TermName}

  /* Modify the template to snag definitions from dynamic context.
   * So object $iw { x + 42 } becomes object $iw { def x = $ctx.x ; x + 42 }
   */
  override protected def importsCode(wanted: Set[Name], request: Request, generousImports: Boolean) = {
    val ImportContextPreamble(exclude, include, scriptedPreamble) =
      importContextPreamble(wanted.filter(_.isTermName).map(_.decodedName.toString))

    if (exclude.nonEmpty) {
      val scriptedWanted = (wanted &~ exclude.map(TermName.apply)) ++ include.map(TermName.apply)

      val ComputedImports(header, preamble, trailer, path) =
        super.importsCode(scriptedWanted, request, generousImports)

      ComputedImports(header, preamble + scriptedPreamble, trailer, path)
    }
    else super.importsCode(wanted, request, generousImports)
  }


  def addBackReferences(req: Request): Either[String, Request] = {
    val defines = req.definesTermNames
    if (defines.isEmpty) {
      recordRequest(new Request(req.line, req.trees, req.parserSource))
      Left(s"new ${req.lineRep.readPath}")
    } else {
      val newReq = requestFromLine(
        (s"val $$INSTANCE = new ${req.lineRep.readPath}" :: (defines map (d =>
            s"val `$d` = $$INSTANCE${req.accessPath}.`$d`"))).mkString(";")
      ).toOption.get
      newReq.compile
      Right(newReq)
    }
  }

  private object scriptContextRep extends ReadEvalPrint
  def call(name: String, args: Any*): Either[Throwable, AnyRef] = scriptContextRep.callEither(name, args: _*)
  def compile(code: String): Boolean = scriptContextRep.compile(code)
  def evalName: String = scriptContextRep.evalName
  def evalPath: String = scriptContextRep.evalPath
}
