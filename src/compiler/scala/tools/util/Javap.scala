/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools
package util

import java.lang.{ ClassLoader => JavaClassLoader, Iterable => JIterable }
import scala.tools.nsc.util.ScalaClassLoader
import scala.tools.nsc.interpreter.IMain
import java.io.{ ByteArrayInputStream, CharArrayWriter, FileNotFoundException, InputStream,
                 PrintWriter, Writer }
import java.util.{ Locale }
import javax.tools.{ Diagnostic, DiagnosticCollector, DiagnosticListener,
                     ForwardingJavaFileManager, JavaFileManager, JavaFileObject,
                     SimpleJavaFileObject, StandardLocation }
import scala.tools.nsc.io.File
import scala.io.Source
import scala.util.{ Try, Success, Failure }
import scala.util.Properties.lineSeparator
import scala.collection.JavaConverters
import scala.collection.generic.Clearable
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
  val loader: ScalaClassLoader,
  val printWriter: PrintWriter,
  intp: Option[IMain] = None
) extends Javap {
  import JavapTool.ToolArgs

  lazy val tool = JavapTool()

  /** Run the tool. Option args start with "-".
   *  The default options are "-protected -verbose".
   *  Byte data for filename args is retrieved with findBytes.
   */
  def apply(args: Seq[String]): List[JpResult] = {
    val (options, claases) = args partition (s => (s startsWith "-") && s.length > 1)
    val (flags, upgraded) = upgrade(options)
    if (flags.help || claases.isEmpty) List(JpResult(JavapTool.helper(printWriter)))
    else tool(flags.raw, upgraded)(claases map (claas => claas -> bytesFor(claas)))
  }

  /** Cull our tool options. */
  private def upgrade(options: Seq[String]): (ToolArgs, Seq[String]) = ToolArgs fromArgs options match {
    case (t,s) if s.nonEmpty  => (t,s)
    case (t,s)                => (t, JavapTool.DefaultOptions)
  }

  private def bytesFor(path: String) = Try {
    def last = intp.get.mostRecentVar  // fail if no intp
    val bytes = findBytes(if (path == "-") last else path)
    if (bytes.isEmpty) throw new FileNotFoundException(s"Could not find class bytes for '${path}'")
    else bytes
  }

  def findBytes(path: String): Array[Byte] = tryFile(path) getOrElse tryClass(path)

  /** Assume the string is a path and try to find the classfile
   *  it represents.
   */
  def tryFile(path: String): Option[Array[Byte]] = {
    val file =
      if (path.endsWith(".class")) path
      else path.replace('.', '/') + ".class"
    (Try (File(file)) filter (_.exists) map (_.toByteArray)).toOption
  }
  /** Assume the string is a fully qualified class name and try to
   *  find the class object it represents.
   */
  def tryClass(path: String): Array[Byte] = {
    def pathology(p: String) =
      if (p endsWith ".class") (p dropRight 6).replace('/', '.')
      else p
    def load(name: String) = loader classBytes pathology(name)
    // if repl, translate the name to something replish
    if (intp.isDefined) {
      val claas = load(intp.get.translatePath(path) getOrElse path)
      if (!claas.isEmpty) claas
      // take path as a Name in scope and find its enclosing class
      else intp.get.translateEnclosingClass(path) match {
        case Some(encl) => load(encl)
        case _          => claas      // empty
      }
    } else load(path)
  }

  abstract class JavapTool {
    type ByteAry = Array[Byte]
    type Input = Pair[String, Try[ByteAry]]
    def apply(raw: Boolean, options: Seq[String])(inputs: Seq[Input]): List[JpResult]
    // Since the tool is loaded by reflection, check for catastrophic failure.
    protected def failed: Boolean
    implicit protected class Failer[A](a: =>A) {
      def orFailed[B >: A](b: =>B) = if (failed) b else a
    }
    protected def noToolError = new JpError(s"No javap tool available: ${getClass.getName} failed to initialize.")
  }

  class JavapTool6 extends JavapTool {
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

    override def apply(raw: Boolean, options: Seq[String])(inputs: Seq[Input]): List[JpResult] =
      (inputs map {
        case (_, Success(ba)) => JpResult(showable(newPrinter(new ByteArrayInputStream(ba), newEnv(options))))
        case (_, Failure(e))  => JpResult(e.toString)
      }).toList orFailed List(noToolError)
  }

  class JavapTool7 extends JavapTool {

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

    class JavaReporter extends DiagnosticListener[JavaFileObject] with Clearable {
      import scala.collection.mutable.{ ArrayBuffer, SynchronizedBuffer }
      type D = Diagnostic[_ <: JavaFileObject]
      val diagnostics = new ArrayBuffer[D] with SynchronizedBuffer[D]
      override def report(d: Diagnostic[_ <: JavaFileObject]) {
        diagnostics += d
      }
      override def clear() = diagnostics.clear()
      /** All diagnostic messages.
       *  @param locale Locale for diagnostic messages, null by default.
       */
      def messages(implicit locale: Locale = null) = (diagnostics map (_ getMessage locale)).toList

      def reportable(raw: Boolean): String = {
        // don't filter this message if raw, since the names are likely to differ
        val container = "Binary file .* contains .*".r
        val m = if (raw) messages
                else messages filter (_ match { case container() => false case _ => true })
        clear()
        if (m.nonEmpty) m mkString ("", lineSeparator, lineSeparator)
        else ""
      }
    }
    val reporter = new JavaReporter

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
    def showable(raw: Boolean): Showable = {
      val written = {
        writer.flush()
        val w = writer.toString
        writer.reset()
        w
      }
      val msgs = reporter.reportable(raw)
      new Showable {
        val mw = msgs + written
        // ReplStrippingWriter clips and scrubs on write(String)
        // circumvent it by write(mw, 0, mw.length) or wrap it in withoutUnwrapping
        def show() =
          if (raw && intp.isDefined) intp.get withoutUnwrapping { writeLines() }
          else writeLines()
        private def writeLines() {
          for (line <- Source.fromString(mw).getLines) printWriter write line+lineSeparator
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
    private def applyOne(raw: Boolean, options: Seq[String], claas: String, inputs: Seq[Input]): Try[JpResult] =
      Try {
        task(options, Seq(claas), inputs).call()
      } map {
        case true => JpResult(showable(raw))
        case _    => JpResult(reporter.reportable(raw))
      } recoverWith {
        case e: java.lang.reflect.InvocationTargetException => e.getCause match {
          case t: IllegalArgumentException => Success(JpResult(t.getMessage)) // bad option
          case x => Failure(x)
        }
      } lastly {
        reporter.clear
      }
    override def apply(raw: Boolean, options: Seq[String])(inputs: Seq[Input]): List[JpResult] = (inputs map {
      case (claas, Success(_))  => applyOne(raw, options, claas, inputs).get
      case (_, Failure(e))      => JpResult(e.toString)
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

    case class ToolArgs(raw: Boolean = false, help: Boolean = false)

    object ToolArgs {
      def fromArgs(args: Seq[String]): (ToolArgs, Seq[String]) = ((ToolArgs(), Seq[String]()) /: (args flatMap massage)) {
        case ((t,others), s) => s match {
          case "-help"  => (t copy (help=true), others)
          case "-raw"   => (t copy (raw=true), others)
          case _        => (t, others :+ s)
        }
      }
    }

    val helps = List(
      "usage"       -> ":javap [opts] [path or class or -]...",
      "-help"       -> "Prints this help message",
      "-raw"        -> "Don't unmangle REPL names",
      "-verbose/-v" -> "Stack size, number of locals, method args",
      "-private/-p" -> "Private classes and members",
      "-package"    -> "Package-private classes and members",
      "-protected"  -> "Protected classes and members",
      "-public"     -> "Public classes and members",
      "-l"          -> "Line and local variable tables",
      "-c"          -> "Disassembled code",
      "-s"          -> "Internal type signatures",
      "-sysinfo"    -> "System info of class",
      "-constants"  -> "Static final constants"
    )

    // match prefixes and unpack opts, or -help on failure
    def massage(arg: String): Seq[String] = {
      require(arg startsWith "-")
      // arg matches opt "-foo/-f" if prefix of -foo or exactly -f
      val r = """(-[^/]*)(/(-.))?""".r
      def maybe(opt: String, s: String): Option[String] = opt match {
        // disambiguate by preferring short form
        case r(lf,_,sf) if s == sf          => Some(sf)
        case r(lf,_,sf) if lf startsWith s  => Some(lf)
        case _ => None
      }
      def candidates(s: String) = (helps map (h => maybe(h._1, s))).flatten
      // one candidate or one single-char candidate
      def uniqueOf(maybes: Seq[String]) = {
        def single(s: String) = s.length == 2
        if (maybes.length == 1) maybes
        else if ((maybes count single) == 1) maybes filter single
        else Nil
      }
      // each optchar must decode to exactly one option
      def unpacked(s: String): Try[Seq[String]] = {
        val ones = (s drop 1) map { c =>
          val maybes = uniqueOf(candidates(s"-$c"))
          if (maybes.length == 1) Some(maybes.head) else None
        }
        Try(ones) filter (_ forall (_.isDefined)) map (_.flatten)
      }
      val res = uniqueOf(candidates(arg))
      if (res.nonEmpty) res
      else (unpacked(arg)
        getOrElse (Seq("-help"))) // or else someone needs help
    }

    def helper(pw: PrintWriter) = new Showable {
      def show() = helps foreach (p => pw write "%-12.12s%s%n".format(p._1,p._2))
    }

    val DefaultOptions = List("-protected", "-verbose")

    def isAvailable = Seq(Env, Tool) exists (cn => hasClass(loader, cn))

    private def hasClass(cl: ScalaClassLoader, cn: String) = cl.tryToInitializeClass[AnyRef](cn).isDefined

    private def isTaskable(cl: ScalaClassLoader) = hasClass(cl, Tool)

    def apply() = if (isTaskable(loader)) new JavapTool7 else new JavapTool6
  }
}

object JavapClass {
  def apply(
    loader: ScalaClassLoader = ScalaClassLoader.appLoader,
    printWriter: PrintWriter = new PrintWriter(System.out, true),
    intp: Option[IMain] = None
  ) = new JavapClass(loader, printWriter, intp)
}

object Javap {

  def isAvailable(cl: ScalaClassLoader = ScalaClassLoader.appLoader) = JavapClass(cl).JavapTool.isAvailable

  def apply(path: String): Unit      = apply(Seq(path))
  def apply(args: Seq[String]): Unit = JavapClass() apply args foreach (_.show())

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
  object JpResult {
    def apply(msg: String)    = new JpError(msg)
    def apply(res: Showable)  = new JpSuccess(res)
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
  implicit class Lastly[A](val t: Try[A]) extends AnyVal {
    private def effect[X](last: =>Unit)(a: X): Try[A] = { last; t }
    def lastly(last: =>Unit): Try[A] = t transform (effect(last) _, effect(last) _)
  }
}
