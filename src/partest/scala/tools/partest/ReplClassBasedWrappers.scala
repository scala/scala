/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Prashant Sharma
 */

package scala.tools.partest

import scala.tools.nsc.interpreter._

/**
 * This changes the standard wrapper generation strategy to generate classes as wrappers instead of objects
 */
class ReplClassBasedWrappers extends ReplAdvancedConfig {

  override def isModule = false

  override def codeWrapper(impname: String, code: StringBuilder) = code append "class %sC extends Serializable {\n".format(impname)

  override def trailBracesWrapper(impname: String, trailingBraces: StringBuilder) = trailingBraces append "}\nval " + impname + " = new " + impname + "C;\n"

  override def accessPathWrapper (impname: String , accessPath: StringBuilder) = accessPath append ("." + impname)

  //Code before and after import wrapper
  override def codeIWrapper(req: IMain#Request, x: MemberHandlers#MemberHandler, code: StringBuilder, symName: String) = {
    val objName = req.lineRep.readPath
    val valName = "$VAL" + newValId();
    if(!code.toString.endsWith(".`" + symName + "`;\n")) { // Which means already imported
      code.append("val " + valName + " = " + objName + ".INSTANCE;\n")
      code.append("import " + valName + req.accessPath + ".`" + symName + "`;\n")
    }
    code
  }

  override def removeExtraUserWrappers(s: String)  = (s.replaceAll("""\$(iw|iwC|read|eval|print)[$.]""", "")).replaceAll("""\$VAL[0-9]+[$.]""", "")

  override def preambleObjSrcCode(lineRep: IMain#ReadEvalPrint, envLines: String, importsPreamble: String, toCompute: String ): String = """
  |class %s extends Serializable {
  | %s%s%s
  """.stripMargin.format(lineRep.readName, envLines, importsPreamble, toCompute)


  override def postambleObjSrcCode(lineRep: IMain#ReadEvalPrint, importsTrailer: String ): String = {
    importsTrailer + "\n}" + "\n" +
    "object " + lineRep.readName + " {\n" +
    "  val INSTANCE = new " + lineRep.readName + "();\n" +
    "}\n"
  }

 override def preambleResObjSrcCode(lineRep: IMain#ReadEvalPrint, evalResult: String, executionWrapper: String, accessPath: String): String = """
      |object %s {
      |  %s
      |  lazy val %s: String = %s {
      |    %s
      |    (""
      """.stripMargin.format(
        lineRep.evalName, evalResult, lineRep.printName,
        executionWrapper, lineRep.readName + ".INSTANCE" + accessPath
      )

  override def fullPath(lineRep: IMain#ReadEvalPrint, accessPath: String, vname: String) = {
    s"${lineRep.readPath}.INSTANCE$accessPath.`$vname`"
  }

  override def originalPathExtended(path: String): String  =  {
    path.replaceFirst("read","read.INSTANCE").replaceAll("iwC","iw")
  }

  private var curValId = 0

  private def newValId(): Int = {
    curValId += 1
    curValId
  }

}
