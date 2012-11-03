/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools
package util

import java.lang.{ ClassLoader => JavaClassLoader, Iterable => JIterable }
import scala.tools.nsc.util.ScalaClassLoader
import java.io.{ ByteArrayInputStream, CharArrayWriter, FileNotFoundException, InputStream,
                 PrintWriter, Writer }
import java.util.{ Locale }
import javax.tools.{ Diagnostic, DiagnosticCollector, DiagnosticListener,
                     ForwardingJavaFileManager, JavaFileManager, JavaFileObject,
                     SimpleJavaFileObject, StandardLocation }
import scala.tools.nsc.io.File
import scala.util.{ Properties, Try, Success, Failure }
import scala.collection.JavaConverters
import scala.language.reflectiveCalls

import Javap._

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

  lazy val tool = JavapTool(loader, printWriter)

  /** Run the tool. Option args start with "-".
   *  The default options are "-protected -verbose".
   *  Byte data for filename args is retrieved with findBytes.
   *  If the filename does not end with ".class", javap will
   *  insert a banner of the form:
   *  `Binary file dummy contains simple.Complex`.
   */
  def apply(args: Seq[String]): List[JpResult] = {
    val (optional, claases) = args partition (_ startsWith "-")
    val options = if (optional.nonEmpty) optional else JavapTool.DefaultOptions
    tool(options)(claases map (claas => claas -> bytesFor(claas)))
  }

  private def bytesFor(path: String) = Try {
    val bytes = findBytes(path)
    if (bytes.isEmpty) throw new FileNotFoundException(s"Could not find class bytes for '${path}'")
    else bytes
  }

  def findBytes(path: String): Array[Byte] = tryFile(path) getOrElse tryClass(path)

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
  def tryClass(path: String): Array[Byte] = loader classBytes {
    if (path endsWith ".class") (path dropRight 6).replace('/', '.')
    else path
  }
}

abstract class JavapTool {
  type ByteAry = Array[Byte]
  type Input = Pair[String, Try[ByteAry]]
  def apply(options: Seq[String])(inputs: Seq[Input]): List[JpResult]
  // Since the tool is loaded by reflection, check for catastrophic failure.
  protected def failed: Boolean
  implicit protected class Failer[A](a: =>A) {
    def orFailed[B >: A](b: =>B) = if (failed) b else a
  }
  protected def noToolError = new JpError(s"No javap tool available: ${getClass.getName} failed to initialize.")
}

class JavapTool6(loader: ScalaClassLoader, printWriter: PrintWriter) extends JavapTool {
  import JavapTool._
  val EnvClass     = loader.tryToInitializeClass[FakeEnvironment](Env).orNull
  val PrinterClass = loader.tryToInitializeClass[FakePrinter](Printer).orNull
  override protected def failed = (EnvClass eq null) || (PrinterClass eq null)

  val PrinterCtr = PrinterClass.getConstructor(classOf[InputStream], classOf[PrintWriter], EnvClass) orFailed null
  def newPrinter(in: InputStream, env: FakeEnvironment): FakePrinter =
    PrinterCtr.newInstance(in, printWriter, env) orFailed null
  def showable(fp: FakePrinter) = new Showable {
    def show() = fp.asInstanceOf[{ def print(): Unit }].print()
  }

  lazy val parser = new JpOptions
  def newEnv(opts: Seq[String]): FakeEnvironment = {
    def result = {
      val env: FakeEnvironment = EnvClass.newInstance()
      parser(opts) foreach { case (name, value) =>
        val field = EnvClass getDeclaredField name
        field setAccessible true
        field.set(env, value.asInstanceOf[AnyRef])
      }
      env
    }
    result orFailed null
  }

  override def apply(options: Seq[String])(inputs: Seq[Input]): List[JpResult] =
    (inputs map {
      case (_, Success(ba)) => new JpSuccess(showable(newPrinter(new ByteArrayInputStream(ba), newEnv(options))))
      case (_, Failure(e))  => new JpError(e.toString)
    }).toList orFailed List(noToolError)
}

class JavapTool7(loader: ScalaClassLoader, printWriter: PrintWriter) extends JavapTool {

  import JavapTool._
  type Task = {
    def call(): Boolean                             // true = ok
    //def run(args: Array[String]): Int             // all args
    //def handleOptions(args: Array[String]): Unit  // options, then run() or call()
  }
  // result of Task.run
  //object TaskResult extends Enumeration {
  //  val Ok, Error, CmdErr, SysErr, Abnormal = Value
  //}
  val TaskClaas = loader.tryToInitializeClass[Task](JavapTool.Tool).orNull
  override protected def failed = TaskClaas eq null

  val TaskCtor  = TaskClaas.getConstructor(
    classOf[Writer],
    classOf[JavaFileManager],
    classOf[DiagnosticListener[_]],
    classOf[JIterable[String]],
    classOf[JIterable[String]]
  ) orFailed null

  val reporter = new DiagnosticCollector[JavaFileObject]

  // DisassemblerTool.getStandardFileManager(reporter,locale,charset)
  val defaultFileManager: JavaFileManager =
    (loader.tryToLoadClass[JavaFileManager]("com.sun.tools.javap.JavapFileManager").get getMethod (
      "create",
      classOf[DiagnosticListener[_]],
      classOf[PrintWriter]
    ) invoke (null, reporter, new PrintWriter(System.err, true))).asInstanceOf[JavaFileManager] orFailed null

  // manages named arrays of bytes, which might have failed to load
  class JavapFileManager(val managed: Seq[Input])(delegate: JavaFileManager = defaultFileManager)
    extends ForwardingJavaFileManager[JavaFileManager](delegate) {
    import JavaFileObject.Kind
    import Kind._
    import StandardLocation._
    import JavaFileManager.Location
    import java.net.URI
    def uri(name: String): URI = new URI(name) // new URI("jfo:" + name)

    def inputNamed(name: String): Try[ByteAry] = (managed find (_._1 == name)).get._2
    def managedFile(name: String, kind: Kind) = kind match {
      case CLASS  => fileObjectForInput(name, inputNamed(name), kind)
      case _      => null
    }
    // todo: just wrap it as scala abstractfile and adapt it uniformly
    def fileObjectForInput(name: String, bytes: Try[ByteAry], kind: Kind): JavaFileObject =
      new SimpleJavaFileObject(uri(name), kind) {
        override def openInputStream(): InputStream = new ByteArrayInputStream(bytes.get)
        // if non-null, ClassWriter wrongly requires scheme non-null
        override def toUri: URI = null
        override def getName: String = name
        // suppress
        override def getLastModified: Long = -1L
      }
    override def getJavaFileForInput(location: Location, className: String, kind: Kind): JavaFileObject =
      location match {
        case CLASS_PATH => managedFile(className, kind)
        case _          => null
      }
    override def hasLocation(location: Location): Boolean =
      location match {
        case CLASS_PATH => true
        case _          => false
      }
  }
  val writer = new CharArrayWriter
  def fileManager(inputs: Seq[Input]) = new JavapFileManager(inputs)()
  def showable(): Showable = {
    val written = {
      writer.flush()
      val w = writer.toString
      writer.reset()
      w
    }
    val msgs = {
      import Properties.lineSeparator
      val m = reporter.messages
      if (m.nonEmpty) m mkString ("", lineSeparator, lineSeparator)
      else ""
    }
    new Showable {
      def show() = {
        val mw = msgs + written
        printWriter.write(mw, 0, mw.length) // ReplStrippingWriter clips on write(String) if truncating
        printWriter.flush()
      }
    }
  }
  // eventually, use the tool interface
  def task(options: Seq[String], claases: Seq[String], inputs: Seq[Input]): Task = {
    //ServiceLoader.load(classOf[javax.tools.DisassemblerTool]).
    //getTask(writer, fileManager, reporter, options.asJava, claases.asJava)
    import JavaConverters.asJavaIterableConverter
    TaskCtor.newInstance(writer, fileManager(inputs), reporter, options.asJava, claases.asJava)
      .orFailed (throw new IllegalStateException)
  }
  // a result per input
  override def apply(options: Seq[String])(inputs: Seq[Input]): List[JpResult] = (inputs map {
    case (claas, Success(ba)) =>
      if (task(options, Seq(claas), inputs).call()) new JpSuccess(showable())
      else new JpError(reporter.messages mkString ",")
    case (_, Failure(e))      => new JpError(e.toString)
  }).toList orFailed List(noToolError)
}

object JavapTool {
  // >= 1.7
  val Tool    = "com.sun.tools.javap.JavapTask"

  // < 1.7
  val Env     = "sun.tools.javap.JavapEnvironment"
  val Printer = "sun.tools.javap.JavapPrinter"
  // "documentation"
  type FakeEnvironment = AnyRef
  type FakePrinter = AnyRef

  // support JavapEnvironment
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

  val DefaultOptions = List("-protected", "-verbose")

  def isAvailable(cl: ScalaClassLoader = ScalaClassLoader.appLoader) = Seq(Env, Tool) exists (cn => hasClass(cl, cn))

  private def hasClass(cl: ScalaClassLoader, cn: String) = cl.tryToInitializeClass[AnyRef](cn).isDefined

  private def isTaskable(cl: ScalaClassLoader) = hasClass(cl, Tool)

  def apply(cl: ScalaClassLoader, pw: PrintWriter) =
    if (isTaskable(cl)) new JavapTool7(cl, pw) else new JavapTool6(cl, pw)

  /** A richer [[javax.tools.DiagnosticCollector]]. */
  implicit class JavaReporter(val c: DiagnosticCollector[JavaFileObject]) extends AnyVal {
    import scala.collection.JavaConverters.iterableAsScalaIterableConverter
    /** All diagnostics in the collector. */
    def diagnostics: Iterable[Diagnostic[_ <: JavaFileObject]] = c.getDiagnostics.asScala
    /** All diagnostic messages.
     *  @param locale Locale for diagnostic messages, null by default.
     */
    def messages(implicit locale: Locale = null) = (diagnostics map (_ getMessage locale)).toList
    /*
    import Diagnostic.Kind.ERROR
    private def isErr(d: Diagnostic[_]) = d.getKind == ERROR
    /** Count the errors. */
    def errorCount: Int = diagnostics count isErr
    /** Error diagnostics in the collector. */
    def errors = (diagnostics filter isErr).toList
    */
  }
}

object Javap {

  def isAvailable(cl: ScalaClassLoader = ScalaClassLoader.appLoader) = JavapTool.isAvailable(cl)

  def apply(path: String): Unit      = apply(Seq(path))
  def apply(args: Seq[String]): Unit = new JavapClass() apply args foreach (_.show())

  trait Showable {
    def show(): Unit
  }

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
    def show() = println(msg)   // makes sense for :javap, less for -Ygen-javap
  }
  class JpSuccess(val value: Showable) extends JpResult {
    type ResultType = AnyRef
    def isError = false
    def show() = value.show()   // output to tool's PrintWriter
  }
}
