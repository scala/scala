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
package shell

import scala.language.reflectiveCalls

import java.io.{InputStream, PrintWriter}
import scala.reflect.internal.util.ScalaClassLoader
import scala.tools.nsc.util.stringFromWriter
import scala.util.{Failure, Success, Try}
import scala.util.{Either, Left, Right}

import Javap.JpResult

/** Javap command implementation.
 */
class JavapClass(
  val loader: ScalaClassLoader,
  intp: Repl,
  tool: JavapTool
) {
  import JavapClass._
  import Javap.{DefaultOptions, HashSplit, helper, toolArgs}
  import JavapTool.Input
  import java.io.FileNotFoundException
  import scala.reflect.io.File

  private val printWriter: PrintWriter = intp.reporter.out

  def apply(args: Seq[String]): List[JpResult] = {
    val (options0, targets) = args.partition(s => s.startsWith("-") && s.length > 1)
    val (options, filter) = {
      val (opts, flag) = toolArgs(options0)
      (if (opts.isEmpty) DefaultOptions else opts, flag)
    }

    if (options.contains("-help") || targets.isEmpty)
      List(JpResult(helper(printWriter)))
    else
      tool(options, filter)(targets.map(targeted))
  }

  /** Associate the requested path with a possibly failed or empty array of bytes. */
  private def targeted(path: String): Input =
    bytesFor(path) match {
      case Success((actual, bytes)) => Input(path, actual, Try(bytes))
      case f: Failure[_]            => Input(path, path, Failure(f.exception))
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
    findBytes(req) match {
      case (_, bytes) if bytes.isEmpty  => throw new FileNotFoundException(s"Could not find class bytes for '$path'")
      case ok @ (actual @ _, bytes @ _) => ok
    }
  }

  // data paired with actual path where it was found
  private def findBytes(path: String): (String, Array[Byte]) = tryFile(path).map(data => (path, data)).getOrElse(tryClass(path))

  /** Assume the string is a path and try to find the classfile it represents.
   */
  private def tryFile(path: String): Option[Array[Byte]] = Try(File(path.asClassResource)).filter(_.exists).map(_.toByteArray()).toOption

  /** Assume the string is a fully qualified class name and try to
   *  find the class object it represents.
   *  There are other symbols of interest, too:
   *  - a definition that is wrapped in an enclosing class
   *  - a synthetic that is not in scope but its associated class is
   */
  private def tryClass(path: String): (String, Array[Byte]) = {
    def load(name: String) = loader.classBytes(name)
    def loadable(name: String) = loader.resourceable(name)
    // if path has an interior dollar, take it as a synthetic
    // if the prefix up to the dollar is a symbol in scope,
    // result is the translated prefix + suffix
    def desynthesize(s: String) = {
      val i = s.indexOf('$')
      if (0 until s.length - 1 contains i) {
        val name = s.substring(0, i)
        val sufx = s.substring(i)
        val tran = intp.translatePath(name)
        def loadableOrNone(strip: Boolean) = {
          def suffix(strip: Boolean)(x: String) =
            (if (strip && x.endsWith("$")) x.init else x) + sufx
          val res = tran.map(suffix(strip)(_))
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
    (q, load(q))
  }
}

object JavapClass {
  private final val classSuffix = ".class"

  // We enjoy flexibility in specifying either a fully-qualified class name com.acme.Widget
  // or a resource path com/acme/Widget.class; but not widget.out
  implicit private class MaybeClassLike(val s: String) extends AnyVal {
    def asClassName = s.stripSuffix(classSuffix).replace('/', '.')
    def asClassResource = if (s.endsWith(classSuffix)) s else s.replace('.', '/') + classSuffix
  }
  implicit private class ClassLoaderOps(val loader: ScalaClassLoader) extends AnyVal {
    /* would classBytes succeed with a nonempty array */
    def resourceable(className: String): Boolean = loader.getResource(className.asClassResource) != null
  }
}

abstract class Javap(protected val intp: Repl) {
  def loader: Either[String, ClassLoader]

  def task(loader: ClassLoader): Either[String, JavapTool]

  /** Run the tool. Option args start with "-", except that "-" itself
   *  denotes the last REPL result.
   *  The default options are "-protected -verbose".
   *  Byte data for filename args is retrieved with findBytes.
   *  @return results for invoking JpResult.show()
   */
  final def apply(args: Seq[String]): List[Javap.JpResult] =
    if (args.isEmpty) List(JpResult(Javap.helpText))
    else
      loader match {
        case Left(msg) => List(JpResult(msg))
        case Right(cl) =>
          task(cl) match {
            case Left(msg) => List(JpResult(msg))
            case Right(tk) => new JavapClass(cl, intp, tk).apply(args)
          }
      }
}

object Javap {
  import scala.util.Properties.isJavaAtLeast
  import java.io.File
  import java.net.URL

  private val javap8 = "scala.tools.nsc.interpreter.shell.Javap8"
  private val javap9 = "scala.tools.nsc.interpreter.shell.Javap9"
  private val javapP = "scala.tools.nsc.interpreter.shell.JavapProvider"

  // load and run a tool
  def apply(intp: Repl)(targets: String*): List[JpResult] = {
    def outDirIsClassPath: Boolean = intp.settings.Yreploutdir.isSetByUser && {
      val outdir = intp.outputDir.file.getAbsoluteFile
      intp.compilerClasspath.exists(url => url.isFile && new File(url.toURI).getAbsoluteFile == outdir)
    }
    def create(toolName: String) = {
      val loader = new ClassLoader(getClass.getClassLoader) with ScalaClassLoader
      loader.create[Javap](toolName, Console.println(_))(intp)
    }
    def advisory = {
      val msg = "On JDK 9 or higher, use -nobootcp to enable :javap, or set -Yrepl-outdir to a file system path on the tool class path with -toolcp."
      List(JpResult(msg))
    }

    if (targets.isEmpty) List(JpResult(Javap.helpText))
    else if (!isJavaAtLeast("9")) create(javap8)(targets)
    else {
      var res: Option[List[JpResult]] = None
      if (classOf[Repl].getClassLoader != null) {
        val javap = create(javap9)
        if (javap.loader.isRight)
          res = Some(javap(targets))
      }
      res.getOrElse {
        if (outDirIsClassPath) create(javapP)(targets)
        else advisory
      }
    }
  }

  implicit private class URLOps(val url: URL) extends AnyVal {
    def isFile: Boolean = url.getProtocol == "file"
  }

  /** Match foo#bar, both groups are optional (may be null). */
  val HashSplit = "([^#]+)?(?:#(.+)?)?".r

  // e.g. Foo#bar. Foo# yields zero-length member part.
  private def splitHashMember(s: String): Option[String] =
    s.lastIndexOf('#') match {
      case -1 => None
      case  i => Some(s.drop(i+1))
    }

  // filter lines of javap output for target such as Klass#methode
  def filterLines(target: String, text: String): String = {
    // take Foo# as Foo#apply for purposes of filtering.
    val filterOn  = splitHashMember(target).map(s => if (s.isEmpty) "apply" else s)
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
    def checkFilter(line: String) = filterOn.map(filterStatus(line, _)).getOrElse(true)
    stringFromWriter(pw => text.linesIterator.foreach(line => if (checkFilter(line)) pw.println(line)))
  }

  private[interpreter] trait Showable {
    def show(): Unit
  }

  /** Create a Showable to show tool messages and tool output, with output massage.
   *  @param filter whether to strip REPL names
   */
  def showable(intp: Repl, filter: Boolean, text: String): Showable =
    new Showable {
      val out = intp.reporter.out
      def show() =
        if (filter) intp.reporter.withoutTruncating(out.write(text))
        else intp.reporter.withoutUnwrapping(out.write(text, 0, text.length))
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

  // split javap options from REPL's -filter flag, also take prefixes of flag names
  def toolArgs(args: Seq[String]): (Seq[String], Boolean) = {
    val (opts, rest) = args.flatMap(massage).partition(_ != "-filter")
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

  def helper(pw: PrintWriter) = new Showable { def show() = pw.print(helpText) }

  val DefaultOptions = List("-protected", "-verbose")
}

/** Loaded reflectively under JDK8 to locate tools.jar and load JavapTask tool. */
class Javap8(intp0: Repl) extends Javap(intp0) {
  import scala.tools.util.PathResolver
  import scala.util.Properties.jdkHome

  private def findToolsJar() = PathResolver.SupplementalLocations.platformTools

  private def addToolsJarToLoader() =
    findToolsJar() match {
      case Some(tools) => ScalaClassLoader.fromURLs(Seq(tools.toURL), intp.classLoader)
      case _           => intp.classLoader
    }
  override def loader =
    Right(addToolsJarToLoader()).filterOrElse(
      _.tryToInitializeClass[AnyRef](JavapTask.taskClassName).isDefined,
      s":javap unavailable: no ${JavapTask.taskClassName} or no tools.jar at $jdkHome"
    )
  override def task(loader: ClassLoader) = Right(new JavapTask(loader, intp))
}

/** Loaded reflectively under JDK9 to load JavapTask tool. */
class Javap9(intp0: Repl) extends Javap(intp0) {
  override def loader =
    Right(new ClassLoader(intp.classLoader) with ScalaClassLoader).filterOrElse(
      _.tryToInitializeClass[AnyRef](JavapTask.taskClassName).isDefined,
      s":javap unavailable: no ${JavapTask.taskClassName}"
    )
  override def task(loader: ClassLoader) = Right(new JavapTask(loader, intp))
}

/** Loaded reflectively under JDK9 to locate ToolProvider. */
class JavapProvider(intp0: Repl) extends Javap(intp0) {
  import JavapTool.Input
  import Javap.filterLines
  import java.util.Optional
  //import java.util.spi.ToolProvider

  type ToolProvider = AnyRef { def run(out: PrintWriter, err: PrintWriter, args: Array[String]): Unit }

  override def loader = Right(getClass.getClassLoader)

  private def tool(provider: ToolProvider) = new JavapTool {
    override def apply(options: Seq[String], filter: Boolean)(inputs: Seq[Input]): List[JpResult] = inputs.map {
      case Input(target, actual, Success(_)) =>
        val more = List("-cp", intp.outputDir.file.getAbsoluteFile.toString, actual)
        val s = stringFromWriter(w => provider.run(w, w, (options ++ more).toArray))
        JpResult(filterLines(target, s))
      case Input(_, _, Failure(e)) => JpResult(e.toString)
    }.toList
  }

  //ToolProvider.findFirst("javap")
  override def task(loader: ClassLoader) = {
    val provider = Class.forName("java.util.spi.ToolProvider", /*initialize=*/ true, loader)
      .getDeclaredMethod("findFirst", classOf[String])
      .invoke(null, "javap").asInstanceOf[Optional[ToolProvider]]
    if (provider.isPresent)
      Right(tool(provider.get))
    else
      Left(s":javap unavailable: provider not found")
  }
}

/** The task or tool provider. */
abstract class JavapTool {
  import JavapTool._
  def apply(options: Seq[String], filter: Boolean)(inputs: Seq[Input]): List[JpResult]
}
object JavapTool {
  case class Input(target: String, actual: String, data: Try[Array[Byte]])
}

// Machinery to run JavapTask reflectively
class JavapTask(val loader: ScalaClassLoader, intp: Repl) extends JavapTool {
  import javax.tools.{DiagnosticListener,
                    ForwardingJavaFileManager, JavaFileManager, JavaFileObject,
                    SimpleJavaFileObject, StandardLocation}
  import java.io.CharArrayWriter
  import scala.jdk.CollectionConverters._
  import JavapTool._
  import Javap.{filterLines, showable}

  // output filtering support
  val writer = new CharArrayWriter
  def written = {
    writer.flush()
    val w = writer.toString
    writer.reset()
    w
  }

  type Task = {
    def call(): Boolean                             // true = ok
    //def run(args: Array[String]): Int             // all args
    //def handleOptions(args: Array[String]): Unit  // options, then run() or call()
  }
  // result of Task.run
  //object TaskResult extends Enumeration {
  //  val Ok, Error, CmdErr, SysErr, Abnormal = Value
  //}

  val reporter = new JavaReporter

  // DisassemblerTool.getStandardFileManager(reporter,locale,charset)
  val defaultFileManager: JavaFileManager =
    (loader.tryToLoadClass[JavaFileManager]("com.sun.tools.javap.JavapFileManager").get.getMethod(
      "create",
      classOf[DiagnosticListener[_]],
      classOf[PrintWriter]
    ).invoke(null, reporter, new PrintWriter(System.err, true))).asInstanceOf[JavaFileManager]

  // manages named arrays of bytes, which might have failed to load
  class JavapFileManager(val managed: Seq[Input])(delegate: JavaFileManager = defaultFileManager)
    extends ForwardingJavaFileManager[JavaFileManager](delegate) {
    import JavaFileObject.Kind
    import Kind._
    import StandardLocation._
    import JavaFileManager.Location
    import java.net.{URI, URISyntaxException}
    import java.io.ByteArrayInputStream

    // name#fragment is OK, but otherwise fragile
    def uri(name: String): URI =
      try new URI(name) // new URI("jfo:" + name)
      catch { case _: URISyntaxException => new URI("dummy") }

    // look up by actual class name or by target descriptor (unused?)
    def inputNamed(name: String): Try[Array[Byte]] = managed.find(m => m.actual == name || m.target == name).get.data

    def managedFile(name: String, kind: Kind) = kind match {
      case CLASS  => fileObjectForInput(name, inputNamed(name), kind)
      case _      => null
    }
    // todo: just wrap it as scala abstractfile and adapt it uniformly
    def fileObjectForInput(name: String, bytes: Try[Array[Byte]], kind: Kind): JavaFileObject =
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

  // eventually, use the tool interface [Edit: which became ToolProvider]
  //ServiceLoader.load(classOf[javax.tools.DisassemblerTool]).
  //getTask(writer, fileManager, reporter, options.asJava, classes.asJava)
  def task(options: Seq[String], classes: Seq[String], inputs: Seq[Input]): Task =
    loader.create[Task](JavapTask.taskClassName, Console.println(_))(writer, fileManager(inputs), reporter, options.asJava, classes.asJava)

  /** Run the tool. */
  override def apply(options: Seq[String], filter: Boolean)(inputs: Seq[Input]): List[JpResult] = inputs.map {
    case Input(target, actual, Success(_)) =>
      import java.lang.reflect.InvocationTargetException
      try {
        if (task(options, Seq(actual), inputs).call()) JpResult(showable(intp, filter, filterLines(target, s"${reporter.reported()}${written}")))
        else JpResult(reporter.reported())
      } catch {
        case e: InvocationTargetException  => e.getCause match {
          case t: IllegalArgumentException => JpResult(t.getMessage) // bad option
          case x => throw x
        }
      } finally {
        reporter.clear()
      }
    case Input(_, _, Failure(e))           => JpResult(e.getMessage)
  }.toList
}

object JavapTask {
  // introduced in JDK7 as internal API
  val taskClassName = "com.sun.tools.javap.JavapTask"
}
