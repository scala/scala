/* NSC -- new scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.reflect
package internal.util

import NameTransformer._
import scala.collection.{ mutable, immutable }
import Origins._

/** A debugging class for logging from whence a method is being called.
 *  Say you wanted to discover who was calling phase_= in SymbolTable.
 *  You could do this:
 *
 *  {{{
 *    private lazy val origins = Origins[SymbolTable]("phase_=")
 *    // Commented out original enclosed for contrast
 *    // final def phase_=(p: Phase): Unit = {
 *    final def phase_=(p: Phase): Unit = origins {
 *  }}}
 *
 *  And that's it.  When the JVM exits it would issue a report something like this:
 {{{
 >> Origins scala.tools.nsc.symtab.SymbolTable.phase_= logged 145585 calls from 51 distinguished sources.

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
  def newRep(xs: StackSlice): Rep
  def repString(rep: Rep): String
  def originClass: String

  private var _tag: String = null
  def tag: String = _tag
  def setTag(tag: String): this.type = {
    _tag = tag
    this
  }

  private val origins      = new mutable.HashMap[Rep, Int] withDefaultValue 0
  private def add(xs: Rep) = origins(xs) += 1
  private def total        = origins.values.foldLeft(0L)(_ + _)

  // We find the right line by dropping any from around here and any
  // from the method's origin class.
  private def dropStackElement(cn: String) =
    (cn startsWith OriginsName) || (cn startsWith originClass)

  // Create a stack and whittle it down to the interesting part.
  private def readStack(): Array[StackTraceElement] =
    (new Throwable).getStackTrace dropWhile (el => dropStackElement(el.getClassName))

  def apply[T](body: => T): T = {
    add(newRep(readStack()))
    body
  }
  def clear() = origins.clear()
  def show()  = {
    println("\n>> Origins %s.%s logged %s calls from %s distinguished sources.\n".format(originClass, tag, total, origins.keys.size))
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
  private type StackSlice = Array[StackTraceElement]
  private val OriginsName = classOf[Origins].getName
  private val counters    = new mutable.HashSet[Origins]

  {
    // Console.println("\nOrigins loaded: registering shutdown hook to display results.")
    sys.addShutdownHook(counters foreach (_.purge()))
  }

  def apply[T: ClassTag](tag: String): Origins = apply(tag, classTag[T].erasure)
  def apply(tag: String, clazz: Class[_]): Origins = apply(tag, new OneLine(clazz))
  def apply(tag: String, orElse: => Origins): Origins = {
    counters find (_.tag == tag) getOrElse {
      val res = orElse setTag tag
      counters += res
      res
    }
  }

  class OneLine(clazz: Class[_]) extends Origins {
    type Rep                        = StackTraceElement
    val originClass                 = clazz.getName stripSuffix MODULE_SUFFIX_STRING
    def newRep(xs: StackSlice): Rep = xs(0)
    def repString(rep: Rep)         = "  " + rep
  }
}
