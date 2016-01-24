/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */
package scala
package tools.nsc
package interpreter

import scala.language.reflectiveCalls

import java.lang.{ Iterable => JIterable }
import scala.reflect.internal.util.ScalaClassLoader
import java.io.{ ByteArrayInputStream, CharArrayWriter, FileNotFoundException, PrintWriter, StringWriter, Writer }
import java.util.{ Locale }
import java.util.concurrent.ConcurrentLinkedQueue
import javax.tools.{ Diagnostic, DiagnosticListener,
                     ForwardingJavaFileManager, JavaFileManager, JavaFileObject,
                     SimpleJavaFileObject, StandardLocation }
import scala.reflect.io.File
import scala.io.Source
import scala.util.{ Try, Success, Failure }
import scala.util.Properties.{ lineSeparator => EOL }
import scala.collection.JavaConverters._
import scala.collection.generic.Clearable
import java.net.URL
import Javap.{ JpResult, JpError, Showable, helper, toolArgs, DefaultOptions }

/** Javap command implementation.
 */
class JavapClass(
  val loader: ScalaClassLoader,
  val printWriter: PrintWriter,
  intp: IMain
) extends Javap {
  import JavapClass._

  lazy val tool = JavapTool()

  def apply(args: Seq[String]): List[JpResult] = {
    val (options0, targets) = args partition (s => (s startsWith "-") && s.length > 1)
    val (options, filter) = {
      val (opts, flag) = toolArgs(options0)
      (if (opts.isEmpty) DefaultOptions else opts, flag)
    }

    if ((options contains "-help") || targets.isEmpty)
      List(JpResult(helper(printWriter)))
    else
      tool(options, filter)(targets map targeted)
  }

  /** Associate the requested path with a possibly failed or empty array of bytes. */
  private def targeted(path: String): (String, Try[Array[Byte]]) =
    bytesFor(path) match {
      case Success((target, bytes)) => (target, Try(bytes))
      case f: Failure[_]            => (path,   Failure(f.exception))
    }

  /** Find bytes. Handle "-", "Foo#bar" (by ignoring member), "#bar" (by taking "bar").
   *  @return the path to use for filtering, and the byte array
   */
  private def bytesFor(path: String) = Try {
    val req = path match {
      case "-"                                    => intp.mostRecentVar
      case HashSplit(prefix, _) if prefix != null => prefix
      case HashSplit(_, member) if member != null => member
      case s                                      => s
    }
    (path, findBytes(req)) match {
      case (_, bytes) if bytes.isEmpty => throw new FileNotFoundException(s"Could not find class bytes for '$path'")
      case ok                          => ok
    }
  }

  def findBytes(path: String): Array[Byte] = tryFile(path) getOrElse tryClass(path)

  /** Assume the string is a path and try to find the classfile it represents.
   */
  def tryFile(path: String): Option[Array[Byte]] =
    (Try (File(path.asClassResource)) filter (_.exists) map (_.toByteArray())).toOption

  /** Assume the string is a fully qualified class name and try to
   *  find the class object it represents.
   *  There are other symbols of interest, too:
   *  - a definition that is wrapped in an enclosing class
   *  - a synthetic that is not in scope but its associated class is
   */
  def tryClass(path: String): Array[Byte] = {
    def load(name: String) = loader classBytes name
    def loadable(name: String) = loader resourceable name
    // if path has an interior dollar, take it as a synthetic
    // if the prefix up to the dollar is a symbol in scope,
    // result is the translated prefix + suffix
    def desynthesize(s: String) = {
      val i = s indexOf '$'
      if (0 until s.length - 1 contains i) {
        val name = s substring (0, i)
        val sufx = s substring i
        val tran = intp translatePath name
        def loadableOrNone(strip: Boolean) = {
          def suffix(strip: Boolean)(x: String) =
            (if (strip && (x endsWith "$")) x.init else x) + sufx
          val res = tran map (suffix(strip) _)
          if (res.isDefined && loadable(res.get)) res else None
        }
        // try loading translated+suffix
        val res = loadableOrNone(strip = false)
        // some synthetics lack a dollar, (e.g., suffix = delayedInit$body)
        // so as a hack, if prefix$$suffix fails, also try prefix$suffix
        if (res.isDefined) res else loadableOrNone(strip = true)
      } else None
    }
    val p = path.asClassName   // scrub any suffix
    // if repl, translate the name to something replish
    // (for translate, would be nicer to get the sym and ask .isClass,
    // instead of translatePath and then asking did I get a class back)
    val q = (
      // only simple names get the scope treatment
      Some(p) filter (_ contains '.')
      // take path as a Name in scope
      orElse (intp translatePath p filter loadable)
      // take path as a Name in scope and find its enclosing class
      orElse (intp translateEnclosingClass p filter loadable)
      // take path as a synthetic derived from some Name in scope
      orElse desynthesize(p)
      // just try it plain
      getOrElse p
    )
    load(q)
  }

  class JavapTool {
    type ByteAry = Array[Byte]
    type Input = Tuple2[String, Try[ByteAry]]

    implicit protected class Failer[A](a: =>A) {
      def orFailed[B >: A](b: =>B) = if (failed) b else a
    }
    protected def noToolError = new JpError(s"No javap tool available: ${getClass.getName} failed to initialize.")

    // output filtering support
    val writer = new CharArrayWriter
    def written = {
      writer.flush()
      val w = writer.toString
      writer.reset()
      w
    }

    def filterLines(target: String, text: String): String = {
      // take Foo# as Foo#apply for purposes of filtering.
      val filterOn  = target.splitHashMember._2 map { s => if (s.isEmpty) "apply" else s }
      var filtering = false   // true if in region matching filter
      // turn filtering on/off given the pattern of interest
      def filterStatus(line: String, pattern: String) = {
        def isSpecialized(method: String) = (method startsWith pattern+"$") && (method endsWith "$sp")
        def isAnonymized(method: String)  = (pattern == "$anonfun") && (method startsWith "$anonfun$")
        // cheap heuristic, todo maybe parse for the java sig.
        // method sigs end in paren semi
        def isAnyMethod = line endsWith ");"
        // take the method name between the space char and left paren.
        // accept exact match or something that looks like what we might be asking for.
        def isOurMethod = {
          val lparen = line lastIndexOf '('
          val blank  = line.lastIndexOf(' ', lparen)
          if (blank < 0) false
          else {
            val method = line.substring(blank+1, lparen)
            (method == pattern || isSpecialized(method) || isAnonymized(method))
          }
        }
        filtering =
          if (filtering) {
            // next blank line terminates section
            // in non-verbose mode, next line is next method, more or less
            line.trim.nonEmpty && (!isAnyMethod || isOurMethod)
          } else {
            isAnyMethod && isOurMethod
          }
        filtering
      }
      // do we output this line?
      def checkFilter(line: String) = filterOn map (filterStatus(line, _)) getOrElse true
      val sw = new StringWriter
      val pw = new PrintWriter(sw)
      for {
        line <- Source.fromString(text).getLines()
        if checkFilter(line)
      } pw println line
      pw.flush()
      sw.toString
    }

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
    val TaskClass = loader.tryToInitializeClass[Task](JavapTask).orNull
    // Since the tool is loaded by reflection, check for catastrophic failure.
    protected def failed = TaskClass eq null

    val TaskCtor  = TaskClass.getConstructor(
      classOf[Writer],
      classOf[JavaFileManager],
      classOf[DiagnosticListener[_]],
      classOf[JIterable[String]],
      classOf[JIterable[String]]
    ) orFailed null

    class JavaReporter extends DiagnosticListener[JavaFileObject] with Clearable {
      type D = Diagnostic[_ <: JavaFileObject]
      val diagnostics = new ConcurrentLinkedQueue[D]
      override def report(d: Diagnostic[_ <: JavaFileObject]) {
        diagnostics add d
      }
      override def clear() = diagnostics.clear()
      /** All diagnostic messages.
       *  @param locale Locale for diagnostic messages, null by default.
       */
      def messages(implicit locale: Locale = null) = diagnostics.asScala.map(_ getMessage locale).toList

      def reportable(): String = {
        clear()
        if (messages.nonEmpty) messages mkString ("", EOL, EOL) else ""
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
      import java.net.{ URI, URISyntaxException }

      // name#fragment is OK, but otherwise fragile
      def uri(name: String): URI =
        try new URI(name) // new URI("jfo:" + name)
        catch { case _: URISyntaxException => new URI("dummy") }

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
    def fileManager(inputs: Seq[Input]) = new JavapFileManager(inputs)()

    /** Create a Showable to show tool messages and tool output, with output massage.
     *  @param target attempt to filter output to show region of interest
     *  @param filter whether to strip REPL names
     */
    def showable(target: String, filter: Boolean): Showable =
      new Showable {
        val output = filterLines(target, s"${reporter.reportable()}${written}")
        def show() =
          if (filter) intp.withoutTruncating(printWriter.write(output))
          else intp.withoutUnwrapping(printWriter.write(output, 0, output.length))
      }

    // eventually, use the tool interface
    def task(options: Seq[String], classes: Seq[String], inputs: Seq[Input]): Task = {
      //ServiceLoader.load(classOf[javax.tools.DisassemblerTool]).
      //getTask(writer, fileManager, reporter, options.asJava, classes.asJava)
      val toolopts = options filter (_ != "-filter")
      TaskCtor.newInstance(writer, fileManager(inputs), reporter, toolopts.asJava, classes.asJava)
        .orFailed (throw new IllegalStateException)
    }
    // a result per input
    private def applyOne(options: Seq[String], filter: Boolean, klass: String, inputs: Seq[Input]): Try[JpResult] =
      Try {
        task(options, Seq(klass), inputs).call()
      } map {
        case true => JpResult(showable(klass, filter))
        case _    => JpResult(reporter.reportable())
      } recoverWith {
        case e: java.lang.reflect.InvocationTargetException => e.getCause match {
          case t: IllegalArgumentException => Success(JpResult(t.getMessage)) // bad option
          case x => Failure(x)
        }
      } lastly {
        reporter.clear()
      }
    /** Run the tool. */
    def apply(options: Seq[String], filter: Boolean)(inputs: Seq[Input]): List[JpResult] = (inputs map {
      case (klass, Success(_))  => applyOne(options, filter, klass, inputs).get
      case (_, Failure(e))      => JpResult(e.toString)
    }).toList orFailed List(noToolError)
  }

  object JavapTool {
    // >= 1.7
    val JavapTask    = "com.sun.tools.javap.JavapTask"

    private def hasClass(cl: ScalaClassLoader, cn: String) = cl.tryToInitializeClass[AnyRef](cn).isDefined

    def isAvailable = hasClass(loader, JavapTask)

    /** Select the tool implementation for this platform. */
    def apply() = {
      require(isAvailable)
      new JavapTool
    }
  }
}

object JavapClass {

  def apply(
    loader: ScalaClassLoader = ScalaClassLoader.appLoader,
    printWriter: PrintWriter = new PrintWriter(System.out, true),
    intp: IMain
  ) = new JavapClass(loader, printWriter, intp)

  /** Match foo#bar, both groups are optional (may be null). */
  val HashSplit = "([^#]+)?(?:#(.+)?)?".r

  // We enjoy flexibility in specifying either a fully-qualified class name com.acme.Widget
  // or a resource path com/acme/Widget.class; but not widget.out
  implicit class MaybeClassLike(val s: String) extends AnyVal {
    /* private[this] final val suffix = ".class" */
    private def suffix = ".class"
    def asClassName = (s stripSuffix suffix).replace('/', '.')
    def asClassResource = if (s endsWith suffix) s else s.replace('.', '/') + suffix
    def splitSuffix: (String, String) = if (s endsWith suffix) (s dropRight suffix.length, suffix) else (s, "")
    def strippingSuffix(f: String => String): String =
      if (s endsWith suffix) f(s dropRight suffix.length) else s
    // e.g. Foo#bar. Foo# yields zero-length member part.
    def splitHashMember: (String, Option[String]) = {
      val i = s lastIndexOf '#'
      if (i < 0) (s, None)
      //else if (i >= s.length - 1) (s.init, None)
      else (s take i, Some(s drop i+1))
    }
  }
  implicit class ClassLoaderOps(val loader: ScalaClassLoader) extends AnyVal {
    /* would classBytes succeed with a nonempty array */
    def resourceable(className: String): Boolean = loader.getResource(className.asClassResource) != null
  }
  implicit class URLOps(val url: URL) extends AnyVal {
    def isFile: Boolean = url.getProtocol == "file"
  }
}

abstract class Javap {
  /** Run the tool. Option args start with "-", except that "-" itself
   *  denotes the last REPL result.
   *  The default options are "-protected -verbose".
   *  Byte data for filename args is retrieved with findBytes.
   *  @return results for invoking JpResult.show()
   */
  def apply(args: Seq[String]): List[Javap.JpResult]
}

object Javap {
  def isAvailable(cl: ScalaClassLoader = ScalaClassLoader.appLoader) = JavapClass(cl, intp = null).JavapTool.isAvailable

  def apply(path: String): Unit      = apply(Seq(path))
  def apply(args: Seq[String]): Unit = JavapClass(intp=null) apply args foreach (_.show())

  private[interpreter] trait Showable {
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

  def toolArgs(args: Seq[String]): (Seq[String], Boolean) = {
    val (opts, rest) = args flatMap massage partition (_ != "-filter")
    (opts, rest.nonEmpty)
  }

  val helps = List(
    "usage"       -> ":javap [opts] [path or class or -]...",
    "-help"       -> "Prints this help message",
    "-verbose/-v" -> "Stack size, number of locals, method args",
    "-private/-p" -> "Private classes and members",
    "-package"    -> "Package-private classes and members",
    "-protected"  -> "Protected classes and members",
    "-public"     -> "Public classes and members",
    "-l"          -> "Line and local variable tables",
    "-c"          -> "Disassembled code",
    "-s"          -> "Internal type signatures",
    "-sysinfo"    -> "System info of class",
    "-constants"  -> "Static final constants",
    "-filter"     -> "Filter REPL machinery from output"
  )

  // match prefixes and unpack opts, or -help on failure
  private def massage(arg: String): Seq[String] = {
    require(arg startsWith "-")
    // arg matches opt "-foo/-f" if prefix of -foo or exactly -f
    val r = """(-[^/]*)(?:/(-.))?""".r
    def maybe(opt: String, s: String): Option[String] = opt match {
      // disambiguate by preferring short form
      case r(lf, sf) if s == sf          => Some(sf)
      case r(lf, sf) if lf startsWith s  => Some(lf)
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

  def helpText: String = (helps map { case (name, help) => f"$name%-12.12s$help%n" }).mkString

  def helper(pw: PrintWriter) = new Showable {
    def show() = pw print helpText
  }

  val DefaultOptions = List("-protected", "-verbose")
}

object NoJavap extends Javap {
  def apply(args: Seq[String]): List[Javap.JpResult] = Nil
}
