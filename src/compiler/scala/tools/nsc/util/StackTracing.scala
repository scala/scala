/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 */

package scala.tools.nsc.util

private[util] trait StackTracing extends Any {

  /** Format a stack trace, returning the prefix consisting of frames that satisfy
   *  a given predicate.
   *  The format is similar to the typical case described in the JavaDoc
   *  for [[java.lang.Throwable#printStackTrace]].
   *  If a stack trace is truncated, it will be followed by a line of the form
   *  `... 3 elided`, by analogy to the lines `... 3 more` which indicate
   *  shared stack trace segments.
   *  @param e the exception
   *  @param p the predicate to select the prefix
   */
  def stackTracePrefixString(e: Throwable)(p: StackTraceElement => Boolean): String = {
    import collection.mutable.{ ArrayBuffer, ListBuffer }
    import compat.Platform.EOL
    import scala.util.Properties.isJavaAtLeast

    val sb = ListBuffer.empty[String]

    type TraceRelation = String
    val Self       = new TraceRelation("")
    val CausedBy   = new TraceRelation("Caused by: ")
    val Suppressed = new TraceRelation("Suppressed: ")

    val suppressable = isJavaAtLeast("1.7")

    def clazz(e: Throwable)           = e.getClass.getName
    def because(e: Throwable): String = e.getCause match { case null => null ; case c => header(c) }
    def msg(e: Throwable): String     = e.getMessage match { case null => because(e) ; case s => s }
    def txt(e: Throwable): String     = msg(e) match { case null => "" ; case s => s": $s" }
    def header(e: Throwable): String  = s"${clazz(e)}${txt(e)}"

    val indent = "\u0020\u0020"

    val seen = new ArrayBuffer[Throwable](16)
    def unseen(t: Throwable) = {
      def inSeen = seen exists (_ eq t)
      val interesting = (t != null) && !inSeen
      if (interesting) seen += t
      interesting
    }

    def print(e: Throwable, r: TraceRelation, share: Array[StackTraceElement], indents: Int): Unit = if (unseen(e)) {
      val trace  = e.getStackTrace
      val frames = (
        if (share.nonEmpty) {
          val spare  = share.reverseIterator
          val trimmed = trace.reverse dropWhile (spare.hasNext && spare.next == _)
          trimmed.reverse
        } else trace
      )
      val prefix   = frames takeWhile p
      val margin   = indent * indents
      val indented = margin + indent
      sb append s"${margin}${r}${header(e)}"
      prefix foreach (f => sb append s"${indented}at $f")
      if (frames.size < trace.size) sb append s"$indented... ${trace.size - frames.size} more"
      if (r == Self && prefix.size < frames.size) sb append s"$indented... ${frames.size - prefix.size} elided"
      print(e.getCause, CausedBy, trace, indents)
      if (suppressable) {
        import scala.language.reflectiveCalls
        type Suppressing = { def getSuppressed(): Array[Throwable] }
        for (s <- e.asInstanceOf[Suppressing].getSuppressed) print(s, Suppressed, frames, indents + 1)
      }
    }
    print(e, Self, share = Array.empty, indents = 0)

    sb mkString EOL
  }
}
