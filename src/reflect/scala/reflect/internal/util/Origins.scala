/* NSC -- new scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala
package reflect
package internal.util

import scala.collection.mutable

/** A debugging class for logging from whence a method is being called.
 *  Say you wanted to discover who was calling phase_= in SymbolTable.
 *  You could do this:
 *
 *  {{{
 *    private lazy val origins = Origins("arbitraryTag")
 *    // Commented out original enclosed for contrast
 *    // final def phase_=(p: Phase): Unit = {
 *    final def phase_=(p: Phase): Unit = origins {
 *  }}}
 *
 *  And that's it.  When the JVM exits it would issue a report something like this:
 {{{
 >> Origins tag 'arbitraryTag' logged 145585 calls from 51 distinguished sources.

   71114   scala.tools.nsc.symtab.Symbols$Symbol.unsafeTypeParams(Symbols.scala:862)
   16584   scala.tools.nsc.symtab.Symbols$Symbol.rawInfo(Symbols.scala:757)
   15411   scala.tools.nsc.symtab.Symbols$Symbol.unsafeTypeParams(Symbols.scala:869)
   11507   scala.tools.nsc.symtab.Symbols$Symbol.rawInfo(Symbols.scala:770)
   10285   scala.tools.nsc.symtab.Symbols$Symbol.unsafeTypeParams(Symbols.scala:864)
    6860   scala.tools.nsc.transform.SpecializeTypes.specializedTypeVars(SpecializeTypes.scala:304)
    ...
 }}}
 *
 */
abstract class Origins {
  type Rep
  type StackSlice = Array[StackTraceElement]

  def tag: String
  def isCutoff(el: StackTraceElement): Boolean
  def newRep(xs: StackSlice): Rep
  def repString(rep: Rep): String

  private val origins      = new mutable.HashMap[Rep, Int] withDefaultValue 0
  private def add(xs: Rep) = origins(xs) += 1
  private def total        = origins.values.foldLeft(0L)(_ + _)

  // Create a stack and whittle it down to the interesting part.
  def readStack(): Array[StackTraceElement] = (
    Thread.currentThread.getStackTrace dropWhile (x => !isCutoff(x)) dropWhile isCutoff drop 1
  )

  def apply[T](body: => T): T = {
    add(newRep(readStack()))
    body
  }
  def clear() = origins.clear()
  def show()  = {
    println("\n>> Origins tag '%s' logged %s calls from %s distinguished sources.\n".format(tag, total, origins.keys.size))
    origins.toList sortBy (-_._2) foreach {
      case (k, v) => println("%7s %s".format(v, repString(k)))
    }
  }
  def purge() = {
    show()
    clear()
  }
}

object Origins {
  private val counters  = mutable.HashMap[String, Origins]()
  private val thisClass = this.getClass.getName

  locally {
    sys.addShutdownHook(counters.values foreach (_.purge()))
  }

  case class OriginId(className: String, methodName: String) {
    def matches(el: StackTraceElement) = (
      (methodName == el.getMethodName) && (className startsWith el.getClassName)
    )
  }

  def lookup(tag: String, orElse: String => Origins): Origins =
    counters.getOrElseUpdate(tag, orElse(tag))
  def register(x: Origins): Origins = {
    counters(x.tag) = x
    x
  }

  private def preCutoff(el: StackTraceElement) = (
       (el.getClassName == thisClass)
    || (el.getClassName startsWith "java.lang.")
  )
  private def findCutoff() = {
    val cutoff = (Thread.currentThread.getStackTrace dropWhile preCutoff).head
    OriginId(cutoff.getClassName, cutoff.getMethodName)
  }

  def apply(tag: String): Origins              = counters.getOrElseUpdate(tag, new OneLine(tag, findCutoff()))
  def apply(tag: String, frames: Int): Origins = counters.getOrElseUpdate(tag, new MultiLine(tag, findCutoff(), frames))

  class OneLine(val tag: String, id: OriginId) extends Origins {
    type Rep                            = StackTraceElement
    def isCutoff(el: StackTraceElement) = id matches el
    def newRep(xs: StackSlice): Rep     = if ((xs eq null) || (xs.length == 0)) null else xs(0)
    def repString(rep: Rep)             = "  " + rep
  }
  class MultiLine(val tag: String, id: OriginId, numLines: Int) extends Origins {
    type Rep                            = List[StackTraceElement]
    def isCutoff(el: StackTraceElement) = id matches el
    def newRep(xs: StackSlice): Rep     = (xs take numLines).toList
    def repString(rep: Rep)             = rep.map("\n  " + _).mkString
    override def readStack()            = super.readStack() drop 1
  }
}
