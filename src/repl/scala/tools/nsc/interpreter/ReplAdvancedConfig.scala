/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Prashant Sharma
 */

package scala
package tools.nsc
package interpreter

class ReplAdvancedConfig {

  /** To tell repl that we are generating wrappper code with object and not classes */
  def isModule = true

  def codeWrapper(impname: String, code: StringBuilder) = code append "object %s {\n".format(impname)

  def trailBracesWrapper(impname: String, trailingBraces: StringBuilder) = trailingBraces append "}\n"

  def accessPathWrapper (impname: String , accessPath: StringBuilder) = accessPath append ("." + impname)

  //Code before and after import wrapper
  def codeIWrapper(req: IMain#Request, x: MemberHandlers#MemberHandler, code: StringBuilder, symName: String) = {
    code append s"import ${x.path}\n"}

  val welcomeMessage: String = ""

  def removeExtraUserWrappers(s: String)  = s

  def preambleObjSrcCode(lineRep: IMain#ReadEvalPrint, envLines: String, importsPreamble: String, toCompute: String ): String = """
  |object %s {
  |%s%s%s
  """.stripMargin.format(lineRep.readName, envLines, importsPreamble, toCompute)

  def preambleResObjSrcCode(lineRep: IMain#ReadEvalPrint, evalResult: String, executionWrapper: String, accessPath: String): String = """
      |object %s {
      |  %s
      |  lazy val %s: String = %s {
      |    %s
      |    (""
      """.stripMargin.format(
        lineRep.evalName, evalResult, lineRep.printName,
        executionWrapper, lineRep.readName + accessPath
      )

  def postambleObjSrcCode(lineRep: IMain#ReadEvalPrint, importsTrailer: String): String = importsTrailer + "\n}"

  def postambleResObjSrcCode(lineRep: IMain#ReadEvalPrint, evalResult: String, executionWrapper: String, accessPath: String): String = """
      |    )
      |  }
      |}
      """.stripMargin

  def fullPath(lineRep: IMain#ReadEvalPrint, accessPath: String, vname: String) = s"${lineRep.readPath}$accessPath.`$vname`"

  def originalPathExtended(path: String): String  = path
}
