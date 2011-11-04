package scala.tools.nsc
package util

import java.util.concurrent.ExecutionException
import java.lang.reflect.{ InvocationTargetException, UndeclaredThrowableException }
import io.{ Sources, Fileish }
import scala.tools.util.StringOps._

/** A simple throwable wrapper so it looks more like a parade of
 *  glittering frame-shaped beauties than the other thing.
 */
class Exceptional(val ex: Throwable)(implicit prefs: ScalaPrefs) {
  val formatter  = prefs.exceptionFormatter(ex)
  val unwrapped  = Exceptional.unwrap(ex)
  val table      = formatter.newTable(unwrapped)
  def rawTrace() = unwrapped.printStackTrace()
  def isScanDone = prefs.codeSources.isDone()

  /** Block until the scanning is complete. */
  def force(): this.type = {
    prefs.codeSources.force()
    this
  }

  /** Stack frame contexts are only shown as long as this is true. */
  def spanFn(frame: JavaStackFrame): Boolean = true

  /** The result of this will be printed before a context trace. */
  def contextPrelude: String =
    if (isScanDone || prefs.codeSources.isEmpty) ""
    else "/* Still scanning source path: there may be more momentarily. */\n"

  /** Frames with surrounding context. */
  private def contextFrames     = toList takeWhile spanFn
  def contextHead(): String     = contextElems.headOption getOrElse ""
  def contextElems()            = contextFrames map formatter.inContext
  def context(): String         = context(length)
  def context(num: Int): String = contextPrelude + ojoinOr(contextFrames take num map formatter.inContext, "\n", "No stack trace.")

  /** Exceptional doesn't extend Seq because it turns out to be super
   *  annoying in the repl: tab-completion sees all the Seq methods.
   */
  def length            = toList.length
  def toList            = table.toList
  def iterator          = table.iterator
  def apply(index: Int) = table(index)

  def causes  = Exceptional.causes(ex)
  def summary = unwrapped.toString + "\n  at " + apply(0).shortNameString

  private def println(msg: Any) = {
    Console println msg
    Console.flush()
  }

  def show(): Unit         = println(context())
  def show(num: Int): Unit = println(context(num))
  def showCauses()         = println((ex :: causes).mkString("", "\n  caused by -> ", ""))
  def showTable()          = println(table)
  def showSummary()        = println(summary)

  override def toString = summary
}


object Exceptional {
  type Catcher[+T] = PartialFunction[Throwable, T]

  /** Creates an exception handler which will only ever catch the given
   *  number of exceptions (if the given pf is defined there) and after
   *  that will disable itself.
   */
  def expiringHandler[T](numCatches: Int)(pf: Catcher[T]): Catcher[T] = {
    var remaining = numCatches;
    { case ex: Throwable if remaining > 0 && pf.isDefinedAt(ex) =>
        remaining -= 1
        pf(ex)
    }
  }

  /** The Throwable => Exceptional implicit plus the associated factory. */
  implicit def throwableToExceptional(ex: Throwable)(implicit prefs: ScalaPrefs): Exceptional = apply(ex)(prefs)
  def apply(ex: Throwable)(implicit prefs: ScalaPrefs) = new Exceptional(ex)(prefs)

  /** Some handy functions. */
  def stack()  = JavaStackFrame frames ((new Throwable).getStackTrace dropWhile isLocal)
  def showme() = apply(new Throwable).show()
  def showstack() = apply(new Throwable).showTable()

  /** A frame formatter with more refined aesthetics than the default.
   *  Come, let us be civilized.
   */
  object ScalaFormat extends TableDef[JavaStackFrame] {
      >> ("file"    -> (_.fileName))   >+ ":"
      << ("line"    -> (_.line))
      >> ("class"   -> (_.shortestName))  >+ "."
      << ("method"  -> (_.methodName))
  }

  trait Calibrated {
    def newTable(ex: Throwable): TableDef[JavaStackFrame]#Table
    def inContext(frame: JavaStackFrame): String
  }
  trait Formatter extends (Throwable => Calibrated) {
    def apply(ex: Throwable): Calibrated
  }
  object Formatter {
    def apply(implicit prefs: ScalaPrefs): Formatter = new Formatter {
      def apply(ex: Throwable) = new Calibrated {
        def newTable(ex: Throwable)          = new ScalaFormat.Table(JavaStackFrame frames ex)
        def inContext(frame: JavaStackFrame) = new FrameContext(frame, prefs.codeSources) toString
      }
    }
  }

  /** Java stack traces have the interesting property of using only the name
   *  of the file, no paths.  This makes it a bit of a gamble to try to associate
   *  a stack frame with a specific file.  Witness the heuristic.
   */
  def locateSources(sources: Sources, frame: JavaStackFrame): List[Fileish] = {
    // if only one has a matching path, that's fairly sure to be the one
    val matches = sources(frame.fileName) filter (_.pkgFromPath endsWith frame.pkgName)
    if (matches.isEmpty || matches.tail.isEmpty)
      return matches

    // otherwise we'll drink them in and look for a package name
    matches filter (_.pkgFromSource endsWith frame.pkgName)
  }

  /** Right now this punts if more than one match and it accepts the first at random.
   */
  def locateSource(sources: Sources, frame: JavaStackFrame): Option[Fileish] =
    locateSources(sources, frame).headOption

  def isLocal(ste: StackTraceElement) = ste.getClassName startsWith this.getClass.getName
  def causes(x: Throwable): List[Throwable] = x.getCause match {
    case null => Nil
    case ex   => x :: causes(ex)
  }
  def unwrap(x: Throwable): Throwable = x match {
    case  _: InvocationTargetException |
          _: ExceptionInInitializerError |
          _: UndeclaredThrowableException |
          _: ExecutionException
            if x.getCause != null =>
              unwrap(x.getCause)

    case _ => x
  }
}
