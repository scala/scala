/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools
package util

import java.lang.reflect.{ GenericSignatureFormatError, Method, Constructor }
import java.lang.{ ClassLoader => JavaClassLoader }
import scala.tools.nsc.util.ScalaClassLoader
import java.io.{ InputStream, PrintWriter, ByteArrayInputStream, FileNotFoundException }
import scala.tools.nsc.io.File
import Javap._
import scala.language.reflectiveCalls

trait Javap {
  def loader: ScalaClassLoader
  def printWriter: PrintWriter
  def apply(args: Seq[String]): List[JpResult]
  def tryFile(path: String): Option[Array[Byte]]
  def tryClass(path: String): Array[Byte]
}

object NoJavap extends Javap {
  def loader: ScalaClassLoader                   = getClass.getClassLoader
  def printWriter: PrintWriter                   = new PrintWriter(System.err, true)
  def apply(args: Seq[String]): List[JpResult]   = Nil
  def tryFile(path: String): Option[Array[Byte]] = None
  def tryClass(path: String): Array[Byte]        = Array()
}

class JavapClass(
  val loader: ScalaClassLoader = ScalaClassLoader.appLoader,
  val printWriter: PrintWriter = new PrintWriter(System.out, true)
) extends Javap {

  lazy val parser = new JpOptions

  val EnvClass     = loader.tryToInitializeClass[FakeEnvironment](Env).orNull
  val PrinterClass = loader.tryToInitializeClass[FakePrinter](Printer).orNull
  private def failed = (EnvClass eq null) || (PrinterClass eq null)

  val PrinterCtr   = (
    if (failed) null
    else PrinterClass.getConstructor(classOf[InputStream], classOf[PrintWriter], EnvClass)
  )

  def findBytes(path: String): Array[Byte] =
    tryFile(path) getOrElse tryClass(path)

  def apply(args: Seq[String]): List[JpResult] = {
    if (failed) List(new JpError("Could not load javap tool. Check that JAVA_HOME is correct."))
    else args.toList filterNot (_ startsWith "-") map { path =>
      val bytes = findBytes(path)
      if (bytes.isEmpty) new JpError("Could not find class bytes for '%s'".format(path))
      else new JpSuccess(newPrinter(new ByteArrayInputStream(bytes), newEnv(args)))
    }
  }

  def newPrinter(in: InputStream, env: FakeEnvironment): FakePrinter =
    if (failed) null
    else PrinterCtr.newInstance(in, printWriter, env)

  def newEnv(opts: Seq[String]): FakeEnvironment = {
    lazy val env: FakeEnvironment = EnvClass.newInstance()

    if (failed) null
    else parser(opts) foreach { case (name, value) =>
      val field = EnvClass getDeclaredField name
      field setAccessible true
      field.set(env, value.asInstanceOf[AnyRef])
    }

    env
  }

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
    loader.classBytes(extName)
  }
}

object Javap {
  val Env     = "sun.tools.javap.JavapEnvironment"
  val Printer = "sun.tools.javap.JavapPrinter"

  def isAvailable(cl: ScalaClassLoader = ScalaClassLoader.appLoader) =
    cl.tryToInitializeClass[AnyRef](Env).isDefined

  // "documentation"
  type FakeEnvironment = AnyRef
  type FakePrinter = AnyRef

  def apply(path: String): Unit      = apply(Seq(path))
  def apply(args: Seq[String]): Unit = new JavapClass() apply args foreach (_.show())

  sealed trait JpResult {
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
  class JpError(msg: String) extends JpResult {
    type ResultType = String
    def isError = true
    def value = msg
    def show() = println(msg)
  }
  class JpSuccess(val value: AnyRef) extends JpResult {
    type ResultType = AnyRef
    def isError = false
    def show() = value.asInstanceOf[{ def print(): Unit }].print()
  }

  class JpOptions {
    private object Access {
      final val PRIVATE = 0
      final val PROTECTED = 1
      final val PACKAGE = 2
      final val PUBLIC = 3
    }
    private val envActionMap: Map[String, (String, Any)] = {
      val map = Map(
        "-l"         -> (("showLineAndLocal", true)),
        "-c"         -> (("showDisassembled", true)),
        "-s"         -> (("showInternalSigs", true)),
        "-verbose"   -> (("showVerbose", true)),
        "-private"   -> (("showAccess", Access.PRIVATE)),
        "-package"   -> (("showAccess", Access.PACKAGE)),
        "-protected" -> (("showAccess", Access.PROTECTED)),
        "-public"    -> (("showAccess", Access.PUBLIC)),
        "-all"       -> (("showallAttr", true))
      )
      map ++ List(
        "-v" -> map("-verbose"),
        "-p" -> map("-private")
      )
    }
    def apply(opts: Seq[String]): Seq[(String, Any)] = {
      opts flatMap { opt =>
        envActionMap get opt match {
          case Some(pair) => List(pair)
          case _          =>
            val charOpts = opt.tail.toSeq map ("-" + _)
            if (charOpts forall (envActionMap contains _))
              charOpts map envActionMap
            else Nil
        }
      }
    }
  }
}
