/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools
package util

import java.io.{ InputStream, PrintWriter, ByteArrayInputStream, FileNotFoundException }
import java.lang.reflect.{ GenericSignatureFormatError, Method, Constructor }
import java.lang.{ ClassLoader => JavaClassLoader }
import scala.tools.nsc.util.ScalaClassLoader
import scala.tools.nsc.io.{ File, NullPrintStream }

trait JavapResult {
  type ResultType
  def isError: Boolean
  def value: ResultType
  def show(): Unit
  // todo
  // def header(): String
  // def fields(): List[String]
  // def methods(): List[String]
  // def signatures(): List[String]
}
class JavapError(msg: String) extends JavapResult {
  type ResultType = String
  def isError = true
  def value = msg
  def show() = println(msg)
}
class JavapSuccess(val value: AnyRef) extends JavapResult {
  type ResultType = AnyRef
  def isError = false
  def show() = value.asInstanceOf[{ def print(): Unit }].print()
}

class Javap(val loader: ScalaClassLoader) {
  def this() = this(ScalaClassLoader.getSystemLoader())
  def defaultPrintWriter = new PrintWriter(System.out, true)

  private val envFieldsToSet = List[(String, Any)](
    // "showLineAndLocal" -> true,
    "showDisassembled" -> true,
    "showVerbose" -> true,
    "showInternalSigs" -> true
  )
  val Env     = "sun.tools.javap.JavapEnvironment"
  val Printer = "sun.tools.javap.JavapPrinter"

  val EnvClass     = loader.tryToInitializeClass[AnyRef](Env).orNull
  val PrinterClass = loader.tryToInitializeClass[AnyRef](Printer).orNull
  val EnvCtr       = EnvClass.getConstructor(List[Class[_]](): _*)
  val PrinterCtr   = PrinterClass.getConstructor(classOf[InputStream], classOf[PrintWriter], EnvClass)

  /** Assume the string is a path and try to find the classfile
   *  it represents.
   */
  def tryFile(path: String): Option[Array[Byte]] = {
    val file = File(
      if (path.endsWith(".class")) path
      else path.replace('.', '/') + ".class"
    )
    if (!file.exists) None
    else try Some(file.toByteArray) catch { case x: Exception => None }
  }
  /** Assume the string is a fully qualified class name and try to
   *  find the class object it represents.
   */
  def tryClass(path: String): Array[Byte] = {
    val extName = (
      if (path endsWith ".class") (path dropRight 6).replace('/', '.')
      else path
    )
    loader.findBytesForClassName(extName)
  }

  def newEnv(): AnyRef = {
    val env = EnvClass.newInstance()
    envFieldsToSet foreach { case (name, value) =>
      val x = EnvClass getDeclaredField name
      x setAccessible true
      x.set(env, value.asInstanceOf[AnyRef])
    }
    env
  }
  def newPrinter(
    in: InputStream,
    pw: PrintWriter = defaultPrintWriter,
    env: AnyRef = newEnv()
  ): AnyRef = {
    PrinterCtr.newInstance(in, pw, env)
  }

  def guess(path: String): JavapResult = {
    val bytes = tryFile(path) getOrElse tryClass(path)
    if (bytes.length > 0) new JavapSuccess(newPrinter(new ByteArrayInputStream(bytes)))
    else new JavapError("Could not find class bytes for '%s'".format(path))
  }
}

object Javap extends Javap(ScalaClassLoader.getSystemLoader()) {
  def apply(path: String): AnyRef = guess(path)
}
