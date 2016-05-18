/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala
package tools.nsc
package interpreter

import java.lang.{ ClassLoader => JavaClassLoader, Iterable => JIterable }
import scala.tools.asm.Opcodes
import scala.tools.nsc.util.ScalaClassLoader
import java.io.{ ByteArrayInputStream, CharArrayWriter, FileNotFoundException, PrintWriter, StringWriter, Writer }
import java.util.{ Locale }
import java.util.concurrent.ConcurrentLinkedQueue
import javax.tools.{ Diagnostic, DiagnosticCollector, DiagnosticListener,
                     ForwardingJavaFileManager, JavaFileManager, JavaFileObject,
                     SimpleJavaFileObject, StandardLocation }
import scala.reflect.io.{ AbstractFile, Directory, File, Path }
import scala.io.Source
import scala.util.{ Try, Success, Failure }
import scala.util.Properties.{ lineSeparator => EOL }
import scala.util.matching.Regex
import scala.collection.JavaConverters._
import scala.collection.generic.Clearable
import java.net.URL
import scala.language.reflectiveCalls
import PartialFunction.{ cond => when }
import Javap._

/** Javap command implementation. Supports platform tool for Java 6 or 7+.
 *  Adds a few options for REPL world, to show bodies of `App` classes and closures.
 */
class JavapClass(
  val loader: ScalaClassLoader,
  val printWriter: PrintWriter,
  intp: Option[IMain] = None
) extends Javap {
  import JavapTool.ToolArgs
  import JavapClass._

  lazy val tool = JavapTool()

  /** Run the tool. Option args start with "-", except that "-" itself
   *  denotes the last REPL result.
   *  The default options are "-protected -verbose".
   *  Byte data for filename args is retrieved with findBytes.
   *  @return results for invoking JpResult.show()
   */
  def apply(args: Seq[String]): List[JpResult] = {
    val (options, classes) = args partition (s => (s startsWith "-") && s.length > 1)
    val (flags, upgraded)  = upgrade(options)
    import flags.{ app, fun, help, raw }

    val targets = if (fun && !help) FunFinder(loader, intp).funs(classes) else classes

    if (help || classes.isEmpty)
      List(JpResult(JavapTool.helper(printWriter)))
    else if (targets.isEmpty)
      List(JpResult("No closures found."))
    else
      tool(raw, upgraded)(targets map (targeted(_, app)))   // JavapTool.apply
  }

  /** Cull our tool options. */
  private def upgrade(options: Seq[String]): (ToolArgs, Seq[String]) =
    ToolArgs fromArgs options match {
      case (t, s) if s.nonEmpty => (t, s)
      case (t, s)               => (t, JavapTool.DefaultOptions)
    }

  /** Associate the requested path with a possibly failed or empty array of bytes. */
  private def targeted(path: String, app: Boolean): (String, Try[Array[Byte]]) =
    bytesFor(path, app) match {
      case Success((target, bytes)) => (target, Try(bytes))
      case f: Failure[_]            => (path,   Failure(f.exception))
    }

  /** Find bytes. Handle "-", "-app", "Foo#bar" (by ignoring member), "#bar" (by taking "bar").
   *  @return the path to use for filtering, and the byte array
   */
  private def bytesFor(path: String, app: Boolean) = Try {
    def last = intp.get.mostRecentVar  // fail if no intp
    val req = path match {
      case "-"                                    => last
      case HashSplit(prefix, _) if prefix != null => prefix
      case HashSplit(_, member) if member != null => member
      case s                                      => s
    }
    val targetedBytes = if (app) findAppBody(req) else (path, findBytes(req))
    targetedBytes match {
      case (_, bytes) if bytes.isEmpty => throw new FileNotFoundException(s"Could not find class bytes for '$path'")
      case ok                          => ok
    }
  }

  private def findAppBody(path: String): (String, Array[Byte]) = {
    // is this new style delayedEndpoint? then find it.
    // the name test is naive. could add $mangled path.
    // assumes only the first match is of interest (because only one endpoint is generated).
    def findNewStyle(bytes: Array[Byte]) = {
      import scala.tools.asm.ClassReader
      //foo/Bar.delayedEndpoint$foo$Bar$1
      val endpoint = "delayedEndpoint".r.unanchored
      def isEndPoint(s: String) = (s contains '$') && when(s) { case endpoint() => true }
      new ClassReader(bytes) withMethods { methods =>
        methods collectFirst { case m if isEndPoint(m.name) => m.name }
      }
    }
    // try new style, and add foo#delayedEndpoint$bar$1 to filter on the endpoint
    def asNewStyle(bytes: Array[Byte]) = Some(bytes) filter (_.nonEmpty) flatMap { bs =>
      findNewStyle(bs) map (n => (s"$path#$n", bs))
    }
    // use old style, and add foo# to filter on apply method
    def asOldStyle = {
      def asAppBody(s: String) = {
        val (cls, fix) = s.splitSuffix
        s"${cls}$$delayedInit$$body${fix}"
      }
      val oldStyle = asAppBody(path)
      val oldBytes = findBytes(oldStyle)
      if (oldBytes.nonEmpty) (s"$oldStyle#", oldBytes)
      else (path, oldBytes)
    }

    val pathBytes = findBytes(path)
    asNewStyle(pathBytes) getOrElse asOldStyle
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
        val tran = intp flatMap (_ translatePath name)
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
    val q = if (intp.isEmpty) p else (
      // only simple names get the scope treatment
      Some(p) filter (_ contains '.')
      // take path as a Name in scope
      orElse (intp flatMap (_ translatePath p) filter loadable)
      // take path as a Name in scope and find its enclosing class
      orElse (intp flatMap (_ translateEnclosingClass p) filter loadable)
      // take path as a synthetic derived from some Name in scope
      orElse desynthesize(p)
      // just try it plain
      getOrElse p
    )
    load(q)
  }

  /** Base class for javap tool adapters for java 6 and 7. */
  abstract class JavapTool {
    type ByteAry = Array[Byte]
    type Input = Tuple2[String, Try[ByteAry]]

    /** Run the tool. */
    def apply(raw: Boolean, options: Seq[String])(inputs: Seq[Input]): List[JpResult]

    // Since the tool is loaded by reflection, check for catastrophic failure.
    protected def failed: Boolean
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
      // take Foo# as Foo#apply for purposes of filtering. Useful for -fun Foo#;
      // if apply is added here, it's for other than -fun: javap Foo#, perhaps m#?
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

    /** Create a Showable with output massage.
     *  @param raw show ugly repl names
     *  @param target attempt to filter output to show region of interest
     *  @param preamble other messages to output
     */
    def showWithPreamble(raw: Boolean, target: String, preamble: String = ""): Showable =
      new Showable {
        private def writeLines() = filterLines(target, preamble + written)
        val output = writeLines()

        // ReplStrippingWriter clips and scrubs on write(String)
        // circumvent it by write(mw, 0, mw.length) or wrap it in withoutUnwrapping
        def show() =
          if (raw && intp.isDefined) intp.get withoutUnwrapping { printWriter.write(output, 0, output.length) }
          else intp.get withoutTruncating(printWriter write output)
      }
  }

  class JavapTool6 extends JavapTool {
    import JavapTool._
    val EnvClass     = loader.tryToInitializeClass[FakeEnvironment](Env).orNull
    val PrinterClass = loader.tryToInitializeClass[FakePrinter](Printer).orNull
    override protected def failed = (EnvClass eq null) || (PrinterClass eq null)

    val PrinterCtr = PrinterClass.getConstructor(classOf[InputStream], classOf[PrintWriter], EnvClass) orFailed null
    val printWrapper = new PrintWriter(writer)
    def newPrinter(in: InputStream, env: FakeEnvironment): FakePrinter =
      PrinterCtr.newInstance(in, printWrapper, env) orFailed null
    def showable(raw: Boolean, target: String, fp: FakePrinter): Showable = {
      fp.asInstanceOf[{ def print(): Unit }].print()      // run tool and flush to buffer
      printWrapper.flush()  // just in case
      showWithPreamble(raw, target)
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
        case (klass, Success(ba)) => JpResult(showable(raw, klass, newPrinter(new ByteArrayInputStream(ba), newEnv(options))))
        case (_, Failure(e))      => JpResult(e.toString)
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
    val TaskClass = loader.tryToInitializeClass[Task](JavapTool.Tool).orNull
    override protected def failed = TaskClass eq null

    val TaskCtor  = TaskClass.getConstructor(
      classOf[Writer],
      classOf[JavaFileManager],
      classOf[DiagnosticListener[_]],
      classOf[JIterable[String]],
      classOf[JIterable[String]]
    ) orFailed null

    class JavaReporter extends DiagnosticListener[JavaFileObject] with Clearable {
      import scala.collection.mutable.{ ArrayBuffer, SynchronizedBuffer }
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

      // don't filter this message if raw, since the names are likely to differ
      private val container = "Binary file .* contains .*".r
      def reportable(raw: Boolean): String = {
        val m = if (raw) messages else messages filterNot (when(_) { case container() => true })
        clear()
        if (m.nonEmpty) m mkString ("", EOL, EOL) else ""
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

    // show tool messages and tool output, with output massage
    def showable(raw: Boolean, target: String): Showable = showWithPreamble(raw, target, reporter.reportable(raw))

    // eventually, use the tool interface
    def task(options: Seq[String], classes: Seq[String], inputs: Seq[Input]): Task = {
      //ServiceLoader.load(classOf[javax.tools.DisassemblerTool]).
      //getTask(writer, fileManager, reporter, options.asJava, classes.asJava)
      TaskCtor.newInstance(writer, fileManager(inputs), reporter, options.asJava, classes.asJava)
        .orFailed (throw new IllegalStateException)
    }
    // a result per input
    private def applyOne(raw: Boolean, options: Seq[String], klass: String, inputs: Seq[Input]): Try[JpResult] =
      Try {
        task(options, Seq(klass), inputs).call()
      } map {
        case true => JpResult(showable(raw, klass))
        case _    => JpResult(reporter.reportable(raw))
      } recoverWith {
        case e: java.lang.reflect.InvocationTargetException => e.getCause match {
          case t: IllegalArgumentException => Success(JpResult(t.getMessage)) // bad option
          case x => Failure(x)
        }
      } lastly {
        reporter.clear()
      }
    override def apply(raw: Boolean, options: Seq[String])(inputs: Seq[Input]): List[JpResult] = (inputs map {
      case (klass, Success(_))  => applyOne(raw, options, klass, inputs).get
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

    case class ToolArgs(raw: Boolean = false, help: Boolean = false, app: Boolean = false, fun: Boolean = false)

    object ToolArgs {
      def fromArgs(args: Seq[String]): (ToolArgs, Seq[String]) = ((ToolArgs(), Seq[String]()) /: (args flatMap massage)) {
        case ((t,others), s) => s match {
          case "-fun"   => (t copy (fun=true), others :+ "-private")
          case "-app"   => (t copy (app=true), others)
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
      "-app"        -> "Show the DelayedInit body of Apps",
      "-fun"        -> "Show anonfuns for class or Class#method",
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

    private def hasClass(cl: ScalaClassLoader, cn: String) = cl.tryToInitializeClass[AnyRef](cn).isDefined

    def isAvailable = Seq(Env, Tool) exists (hasClass(loader, _))

    /** Select the tool implementation for this platform. */
    def apply() = if (hasClass(loader, Tool)) new JavapTool7 else new JavapTool6
  }
}

object JavapClass {
  import scala.tools.asm.ClassReader
  import scala.tools.asm.tree.{ ClassNode, MethodNode }

  def apply(
    loader: ScalaClassLoader = ScalaClassLoader.appLoader,
    printWriter: PrintWriter = new PrintWriter(System.out, true),
    intp: Option[IMain] = None
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
    private def parentsOf(x: ClassLoader): List[ClassLoader] = if (x == null) Nil else x :: parentsOf(x.getParent)
    def parents: List[ClassLoader] = parentsOf(loader)
    /* all file locations */
    def locations = {
      def alldirs = parents flatMap {
        case ucl: ScalaClassLoader.URLClassLoader => ucl.classPathURLs
        case jcl: java.net.URLClassLoader         => jcl.getURLs
        case _ => Nil
      }
      val dirs = for (d <- alldirs; if d.getProtocol == "file") yield Path(new JFile(d.toURI))
      dirs
    }
    /* only the file location from which the given class is loaded */
    def locate(k: String): Option[Path] = {
      Try {
        val klass = try loader loadClass k catch {
          case _: NoClassDefFoundError => null    // let it snow
        }
        // cf ScalaClassLoader.originOfClass
        klass.getProtectionDomain.getCodeSource.getLocation
      } match {
        case Success(null)              => None
        case Success(loc) if loc.isFile => Some(Path(new JFile(loc.toURI)))
        case _                          => None
      }
    }
    /* would classBytes succeed with a nonempty array */
    def resourceable(className: String): Boolean = loader.getResource(className.asClassResource) != null

    /* class reader of class bytes */
    def classReader(resource: String): ClassReader = new ClassReader(loader classBytes resource)
  }
  implicit class `class reader convenience`(val reader: ClassReader) extends AnyVal {
    def withMethods[A](f: Seq[MethodNode] => A): A = {
      val cls = new ClassNode
      reader.accept(cls, 0)
      f(cls.methods.asScala)
    }
  }
  implicit class PathOps(val p: Path) extends AnyVal {
    import scala.tools.nsc.io.Jar
    def isJar = Jar isJarOrZip p
  }
  implicit class `fun with files`(val f: AbstractFile) extends AnyVal {
    def descend(path: Seq[String]): Option[AbstractFile] = {
      def lookup(f: AbstractFile, path: Seq[String]): Option[AbstractFile] = path match {
        case p if p.isEmpty => Option(f)
        case p => Option(f.lookupName(p.head, directory = true)) flatMap (lookup(_, p.tail))
      }
      lookup(f, path)
    }
  }
  implicit class URLOps(val url: URL) extends AnyVal {
    def isFile: Boolean = url.getProtocol == "file"
  }
  object FunFinder {
    def apply(loader: ScalaClassLoader, intp: Option[IMain]) = new FunFinder(loader, intp)
  }
  // FunFinder.funs(ks) finds anonfuns
  class FunFinder(loader: ScalaClassLoader, intp: Option[IMain]) {

    // manglese for closure: typename, $anonfun or lambda, opt method, digits
    val closure = """(.*)\$(\$anonfun|lambda)(?:\$+([^$]+))?\$(\d+)""".r

    // manglese for closure
    val cleese = "(?:anonfun|lambda)"

    // class k, candidate f without prefix
    def isFunOfClass(k: String, f: String) = (s"${Regex quote k}\\$$+$cleese".r findPrefixOf f).nonEmpty

    // class k, candidate f without prefix, method m
    def isFunOfMethod(k: String, m: String, f: String) =
      (s"${Regex quote k}\\$$+$cleese\\$$+${Regex quote m}\\$$".r findPrefixOf f).nonEmpty

    def isFunOfTarget(target: Target, f: String) =
      target.member map (isFunOfMethod(target.name, _, f)) getOrElse isFunOfClass(target.name, f)

    def listFunsInAbsFile(target: Target)(d: AbstractFile) =
      for (f <- d; if !f.isDirectory && isFunOfTarget(target, f.name)) yield f.name

    def listFunsInDir(target: Target)(d: Directory) = {
      val subdir = Path(target.prefix)
      for (f <- (d / subdir).toDirectory.list; if f.isFile && isFunOfTarget(target, f.name))
        yield f.name
    }

    def listFunsInJar(target: Target)(f: File) = {
      import java.util.jar.JarEntry
      import scala.tools.nsc.io.Jar
      def maybe(e: JarEntry) = {
        val (path, name) = {
          val parts = e.getName split "/"
          if (parts.length < 2) ("", e.getName)
          else (parts.init mkString "/", parts.last)
        }
        if (path == target.prefix && isFunOfTarget(target, name)) Some(name) else None
      }
      (new Jar(f) map maybe).flatten
    }
    def loadable(name: String) = loader resourceable name
    case class Target(path: String, member: Option[String], filter: Option[String], isRepl: Boolean, isModule: Boolean) {
      val splat  = path split "\\."
      val name   = splat.last
      val prefix = if (splat.length > 1) splat.init mkString "/" else ""
      val pkg    = if (splat.length > 1) splat.init mkString "." else ""
      val targetName = s"$name${ if (isModule) "$" else "" }"
    }
    // translated class, optional member, opt member to filter on, whether it is repl output and a module
    def translate(s: String): Target = {
      val (k0, m0) = s.splitHashMember
      val isModule = k0 endsWith "$"
      val k = (k0 stripSuffix "$").asClassName
      val member = m0 filter (_.nonEmpty)  // take Foo# as no member, not ""
      val filter = m0 flatMap { case "" => Some("apply") case _ => None }   // take Foo# as filter on apply
      // class is either something replish or available to loader
      // $line.$read$$etc$Foo#member
      ((intp flatMap (_ translatePath k) filter (loadable) map (x => Target(x stripSuffix "$", member, filter, true, isModule)))
      // s = "f" and $line.$read$$etc$#f is what we're after,
      // ignoring any #member (except take # as filter on #apply)
      orElse (intp flatMap (_ translateEnclosingClass k) map (x => Target(x stripSuffix "$", Some(k), filter, true, isModule)))
      getOrElse (Target(k, member, filter, false, isModule)))
    }
    /** Find the classnames of anonfuns associated with k,
     *  where k may be an available class or a symbol in scope.
     */
    def funsOf(selection: String): Seq[String] = {
      // class is either something replish or available to loader
      val target = translate(selection)

      // reconstitute an anonfun with a package
      // if filtered, add the hash back, e.g. pkg.Foo#bar, pkg.Foo$anon$1#apply
      def packaged(s: String) = {
        val p = if (target.pkg.isEmpty) s else s"${target.pkg}.$s"
        target.filter map (p + "#" + _) getOrElse p
      }
      // find closure classes in repl outdir or try asking the classloader where to look
      val fs =
        if (target.isRepl)
          (intp.get.replOutput.dir descend target.splat.init) map { d =>
            listFunsInAbsFile(target)(d) map (_.asClassName) map packaged
          }
        else
          loader locate target.path map {
            case d if d.isDirectory => listFunsInDir(target)(d.toDirectory) map packaged
            case j if j.isJar       => listFunsInJar(target)(j.toFile) map packaged
            case _                  => Nil
          }
      val res = fs map (_.to[Seq]) getOrElse Seq()
      // on second thought, we don't care about lambda method classes, just the impl methods
      val rev =
      res flatMap {
        case x @ closure(_, "lambda", _, _) => lambdaMethod(x, target)
          //target.member flatMap (_ => lambdaMethod(x, target)) getOrElse s"${target.name}#$$anonfun"
        case x                              => Some(x)
      }
      rev
    }
    // given C$lambda$$g$n for member g and n in 1..N, find the C.accessor$x
    // and the C.$anonfun$x it forwards to.
    def lambdaMethod(lambda: String, target: Target): Option[String] = {
      import scala.tools.asm.ClassReader
      import scala.tools.asm.Opcodes.INVOKESTATIC
      import scala.tools.asm.tree.{ ClassNode, MethodInsnNode }
      def callees(s: String): List[(String, String)] = {
        loader classReader s withMethods { ms =>
          val nonBridgeApplyMethods = ms filter (_.name == "apply") filter (n => (n.access & Opcodes.ACC_BRIDGE) == 0)
          val instructions = nonBridgeApplyMethods flatMap (_.instructions.toArray)
          instructions.collect {
            case i: MethodInsnNode => (i.owner, i.name)
          }.toList
        }
      }
      callees(lambda) match {
        case (k, _) :: Nil if target.isModule && !(k endsWith "$") => None
        case (k, m) :: _ => Some(s"${k}#${m}")
        case _ => None
      }
    }
    /** Translate the supplied targets to patterns for anonfuns.
     *  Pattern is typename $ label [[$]$func] $n where label is $anonfun or lambda,
     *  and lambda includes the extra dollar, func is a method name, and n is an int.
     *  The typename for a nested class is dollar notation, Betty$Bippy.
     *
     *  If C has anonfun closure classes, then use C$$anonfun$f$1 (various names, C# filters on apply).
     *  If C has lambda closure classes, then use C#$anonfun (special-cased by output filter).
     */
    def funs(ks: Seq[String]): Seq[String] = ks flatMap funsOf
  }
}

trait Javap {
  def loader: ScalaClassLoader
  def printWriter: PrintWriter
  def apply(args: Seq[String]): List[Javap.JpResult]
  def tryFile(path: String): Option[Array[Byte]]
  def tryClass(path: String): Array[Byte]
}

object Javap {
  def isAvailable(cl: ScalaClassLoader = ScalaClassLoader.appLoader) = JavapClass(cl).JavapTool.isAvailable

  def apply(path: String): Unit      = apply(Seq(path))
  def apply(args: Seq[String]): Unit = JavapClass() apply args foreach (_.show())

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
}

object NoJavap extends Javap {
  import Javap._
  def loader: ScalaClassLoader                   = getClass.getClassLoader
  def printWriter: PrintWriter                   = new PrintWriter(System.err, true)
  def apply(args: Seq[String]): List[JpResult]   = Nil
  def tryFile(path: String): Option[Array[Byte]] = None
  def tryClass(path: String): Array[Byte]        = Array()
}
