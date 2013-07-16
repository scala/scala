/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala
package tools.nsc
package interpreter

import java.lang.{ ClassLoader => JavaClassLoader, Iterable => JIterable }
import scala.tools.nsc.util.ScalaClassLoader
import java.io.{ ByteArrayInputStream, CharArrayWriter, FileNotFoundException, PrintWriter, Writer }
import java.util.{ Locale }
import java.util.regex.Pattern
import javax.tools.{ Diagnostic, DiagnosticCollector, DiagnosticListener,
                     ForwardingJavaFileManager, JavaFileManager, JavaFileObject,
                     SimpleJavaFileObject, StandardLocation }
import scala.reflect.io.{ AbstractFile, Directory, File, Path }
import scala.io.Source
import scala.util.{ Try, Success, Failure }
import scala.util.Properties.lineSeparator
import scala.collection.JavaConverters
import scala.collection.generic.Clearable
import java.net.URL
import scala.language.reflectiveCalls
import Javap._

class JavapClass(
  val loader: ScalaClassLoader,
  val printWriter: PrintWriter,
  intp: Option[IMain] = None
) extends scala.tools.util.Javap {
  import JavapTool.ToolArgs
  import JavapClass._

  lazy val tool = JavapTool()

  /** Run the tool. Option args start with "-".
   *  The default options are "-protected -verbose".
   *  Byte data for filename args is retrieved with findBytes.
   */
  def apply(args: Seq[String]): List[JpResult] = {
    val (options, claases) = args partition (s => (s startsWith "-") && s.length > 1)
    val (flags, upgraded) = upgrade(options)
    import flags.{ app, fun, help, raw }
    val targets = if (fun && !help) FunFinder(loader, intp).funs(claases) else claases
    if (help || claases.isEmpty)
      List(JpResult(JavapTool.helper(printWriter)))
    else if (targets.isEmpty)
      List(JpResult("No anonfuns found."))
    else
      tool(raw, upgraded)(targets map (claas => targeted(claas, app)))
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

  /** Find bytes. Handle "-", "-app", "Foo#bar" (by ignoring member), "#bar" (by taking "bar"). */
  private def bytesFor(path: String, app: Boolean) = Try {
    def last = intp.get.mostRecentVar  // fail if no intp
    def req = path match {
      case "-" => last
      case HashSplit(prefix, member) =>
        if (prefix != null) prefix
        else if (member != null) member
        else "#"
    }
    val targetedBytes = if (app) findAppBody(req) else (req, findBytes(req))
    if (targetedBytes._2.isEmpty) throw new FileNotFoundException(s"Could not find class bytes for '$path'")
    targetedBytes
  }

  private def findAppBody(path: String): (String, Array[Byte]) = {
    // is this new style delayedEndpoint? then find it.
    // the name test is naive. could add $mangled path.
    // assumes only the first match is of interest (because only one endpoint is generated).
    def findNewStyle(bytes: Array[Byte]) = {
      import scala.tools.asm.ClassReader
      import scala.tools.asm.tree.ClassNode
      import PartialFunction.cond
      import JavaConverters._
      val rdr = new ClassReader(bytes)
      val nod = new ClassNode
      rdr.accept(nod, 0)
      //foo/Bar.delayedEndpoint$foo$Bar$1
      val endpoint = "delayedEndpoint".r.unanchored
      def isEndPoint(s: String) = (s contains '$') && cond(s) { case endpoint() => true }
      nod.methods.asScala collectFirst { case m if isEndPoint(m.name) => m.name }
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

  /** Assume the string is a path and try to find the classfile
   *  it represents.
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
    type Input = Pair[String, Try[ByteAry]]

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

    /** Create a Showable with output massage.
     *  @param raw show ugly repl names
     *  @param target attempt to filter output to show region of interest
     *  @param preamble other messages to output
     */
    def showWithPreamble(raw: Boolean, target: String, preamble: String = ""): Showable = new Showable {
      // ReplStrippingWriter clips and scrubs on write(String)
      // circumvent it by write(mw, 0, mw.length) or wrap it in withoutUnwrapping
      def show() =
        if (raw && intp.isDefined) intp.get withoutUnwrapping { writeLines() }
        else writeLines()
      private def writeLines() {
        // take Foo# as Foo#apply for purposes of filtering. Useful for -fun Foo#;
        // if apply is added here, it's for other than -fun: javap Foo#, perhaps m#?
        val filterOn = target.splitHashMember._2 map { s => if (s.isEmpty) "apply" else s }
        var filtering = false   // true if in region matching filter
        // true to output
        def checkFilter(line: String) = if (filterOn.isEmpty) true else {
          // cheap heuristic, todo maybe parse for the java sig.
          // method sigs end in paren semi
          def isAnyMethod = line.endsWith(");")
          def isOurMethod = {
            val lparen = line.lastIndexOf('(')
            val blank = line.lastIndexOf(' ', lparen)
            (blank >= 0 && line.substring(blank+1, lparen) == filterOn.get)
          }
          filtering = if (filtering) {
            // next blank line terminates section
            // for -public, next line is next method, more or less
            line.trim.nonEmpty && !isAnyMethod
          } else {
            isAnyMethod && isOurMethod
          }
          filtering
        }
        for (line <- Source.fromString(preamble + written).getLines(); if checkFilter(line))
          printWriter write line+lineSeparator
        printWriter.flush()
      }
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
        case (claas, Success(ba)) => JpResult(showable(raw, claas, newPrinter(new ByteArrayInputStream(ba), newEnv(options))))
        case (_, Failure(e))      => JpResult(e.toString)
      }).toList orFailed List(noToolError)
  }

  class JavapTool7 extends JavapTool {
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
    def fileManager(inputs: Seq[Input]) = new JavapFileManager(inputs)()

    // show tool messages and tool output, with output massage
    def showable(raw: Boolean, target: String): Showable = showWithPreamble(raw, target, reporter.reportable(raw))

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
        case true => JpResult(showable(raw, claas))
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

    case class ToolArgs(raw: Boolean = false, help: Boolean = false, app: Boolean = false, fun: Boolean = false)

    object ToolArgs {
      def fromArgs(args: Seq[String]): (ToolArgs, Seq[String]) = ((ToolArgs(), Seq[String]()) /: (args flatMap massage)) {
        case ((t,others), s) => s match {
          case "-fun"   => (t copy (fun=true), others)
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

  val HashSplit = "(.*?)(?:#([^#]*))?".r
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
  implicit class ClassLoaderOps(val cl: ClassLoader) extends AnyVal {
    private def parentsOf(x: ClassLoader): List[ClassLoader] = if (x == null) Nil else x :: parentsOf(x.getParent)
    def parents: List[ClassLoader] = parentsOf(cl)
    /* all file locations */
    def locations = {
      def alldirs = parents flatMap (_ match {
        case ucl: ScalaClassLoader.URLClassLoader => ucl.classPathURLs
        case jcl: java.net.URLClassLoader         => jcl.getURLs
        case _ => Nil
      })
      val dirs = for (d <- alldirs; if d.getProtocol == "file") yield Path(new JFile(d.toURI))
      dirs
    }
    /* only the file location from which the given class is loaded */
    def locate(k: String): Option[Path] = {
      Try {
        val claas = try cl loadClass k catch {
          case _: NoClassDefFoundError => null    // let it snow
        }
        // cf ScalaClassLoader.originOfClass
        claas.getProtectionDomain.getCodeSource.getLocation
      } match {
        case Success(null)              => None
        case Success(loc) if loc.isFile => Some(Path(new JFile(loc.toURI)))
        case _                          => None
      }
    }
    /* would classBytes succeed with a nonempty array */
    def resourceable(className: String): Boolean = cl.getResource(className.asClassResource) != null
  }
  implicit class PathOps(val p: Path) extends AnyVal {
    import scala.tools.nsc.io.Jar
    def isJar = Jar isJarOrZip p
  }
  implicit class URLOps(val url: URL) extends AnyVal {
    def isFile: Boolean = url.getProtocol == "file"
  }
  object FunFinder {
    def apply(loader: ScalaClassLoader, intp: Option[IMain]) = new FunFinder(loader, intp)
  }
  class FunFinder(loader: ScalaClassLoader, intp: Option[IMain]) {

    // class k, candidate f without prefix
    def isFunOfClass(k: String, f: String) = {
      val p = (s"${Pattern quote k}\\$$+anonfun").r
      (p findPrefixOf f).nonEmpty
    }
    // class k, candidate f without prefix, method m
    def isFunOfMethod(k: String, m: String, f: String) = {
      val p = (s"${Pattern quote k}\\$$+anonfun\\$$${Pattern quote m}\\$$").r
      (p findPrefixOf f).nonEmpty
    }
    def isFunOfTarget(k: String, m: Option[String], f: String) =
      if (m.isEmpty) isFunOfClass(k, f)
      else isFunOfMethod(k, m.get, f)
    def listFunsInAbsFile(k: String, m: Option[String], d: AbstractFile) = {
      for (f <- d; if !f.isDirectory && isFunOfTarget(k, m, f.name)) yield f.name
    }
    // path prefix p, class k, dir d
    def listFunsInDir(p: String, k: String, m: Option[String])(d: Directory) = {
      val subdir  = Path(p)
      for (f <- (d / subdir).toDirectory.list; if f.isFile && isFunOfTarget(k, m, f.name))
        yield f.name
    }
    // path prefix p, class k, jar file f
    def listFunsInJar(p: String, k: String, m: Option[String])(f: File) = {
      import java.util.jar.JarEntry
      import scala.tools.nsc.io.Jar
      def maybe(e: JarEntry) = {
        val (path, name) = {
          val parts = e.getName split "/"
          if (parts.length < 2) ("", e.getName)
          else (parts.init mkString "/", parts.last)
        }
        if (path == p && isFunOfTarget(k, m, name)) Some(name) else None
      }
      (new Jar(f) map maybe).flatten
    }
    def loadable(name: String) = loader resourceable name
    // translated class, optional member, opt member to filter on, whether it is repl output
    def translate(s: String): (String, Option[String], Option[String], Boolean) = {
      val (k0, m0) = s.splitHashMember
      val k = k0.asClassName
      val member = m0 filter (_.nonEmpty)  // take Foo# as no member, not ""
      val filter = m0 flatMap { case "" => Some("apply") case _ => None }   // take Foo# as filter on apply
      // class is either something replish or available to loader
      // $line.$read$$etc$Foo#member
      ((intp flatMap (_ translatePath k) filter (loadable) map ((_, member, filter, true)))
      // s = "f" and $line.$read$$etc$#f is what we're after,
      // ignoring any #member (except take # as filter on #apply)
      orElse (intp flatMap (_ translateEnclosingClass k) map ((_, Some(k), filter, true)))
      getOrElse ((k, member, filter, false)))
    }
    /** Find the classnames of anonfuns associated with k,
     *  where k may be an available class or a symbol in scope.
     */
    def funsOf(k0: String): Seq[String] = {
      // class is either something replish or available to loader
      val (k, member, filter, isReplish) = translate(k0)
      val splat   = k split "\\."
      val name    = splat.last
      val prefix  = if (splat.length > 1) splat.init mkString "/" else ""
      val pkg     = if (splat.length > 1) splat.init mkString "." else ""
      // reconstitute an anonfun with a package
      // if filtered, add the hash back, e.g. pkg.Foo#bar, pkg.Foo$anon$1#apply
      def packaged(s: String) = {
        val p = if (pkg.isEmpty) s else s"$pkg.$s"
        val pm = filter map (p + "#" + _)
        pm getOrElse p
      }
      // is this translated path in (usually virtual) repl outdir? or loadable from filesystem?
      val fs = if (isReplish) {
        def outed(d: AbstractFile, p: Seq[String]): Option[AbstractFile] = {
          if (p.isEmpty) Option(d)
          else Option(d.lookupName(p.head, directory = true)) flatMap (f => outed(f, p.tail))
        }
        outed(intp.get.replOutput.dir, splat.init) map { d =>
          listFunsInAbsFile(name, member, d) map packaged
        }
      } else {
        loader locate k map { w =>
          if (w.isDirectory) listFunsInDir(prefix, name, member)(w.toDirectory) map packaged
          else if (w.isJar) listFunsInJar(prefix, name, member)(w.toFile) map packaged
          else Nil
        }
      }
      fs match {
        case Some(xs) => xs.to[Seq]     // maybe empty
        case None     => Seq()          // nothing found, e.g., junk input
      }
    }
    def funs(ks: Seq[String]) = ks flatMap funsOf _
  }
}

object Javap {
  def isAvailable(cl: ScalaClassLoader = ScalaClassLoader.appLoader) = JavapClass(cl).JavapTool.isAvailable

  def apply(path: String): Unit      = apply(Seq(path))
  def apply(args: Seq[String]): Unit = JavapClass() apply args foreach (_.show())

  trait Showable {
    def show(): Unit
  }

  sealed trait JpResult extends scala.tools.util.JpResult {
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
