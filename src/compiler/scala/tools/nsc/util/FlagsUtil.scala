/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package util

// Overloading invariants: these are "pseudoinvariants" because many of the
// methods do not exist on Modifiers, only on Symbol, not to mention it is only
// speculative that they are mutually exclusive: but is here to provide a
// starting point for further refinement.
//
// 16: BYNAMEPARAM CAPTURED COVARIANT
//   x.isParameter  ==> BYNAMEPARAM
//   x.isMutable    ==> CAPTURED
//   x.isType       ==> COVARIANT
//
// 17: CONTRAVARIANT INCONSTRUCTOR LABEL
//   x.isType       ==> CONTRAVARIANT
//   x.isClass      ==> INCONSTRUCTOR
//   x.isMethod     ==> LABEL
//
// 25: DEFAULTPARAM TRAIT
//   x.isParameter  ==> DEFAULTPARAM
//   x.isClass      ==> TRAIT
//
// 35: EXISTENTIAL MIXEDIN
//   x.isType       ==> EXISTENTIAL
//   x.isTerm       ==> MIXEDIN
//
// 37: IMPLCLASS PRESUPER
//   x.isClass      ==> IMPLCLASS
//   x.isTerm       ==> PRESUPER

import scala.collection.{ mutable, immutable }
import symtab.Flags.ExplicitFlags

class TransFlagManager[T <: Global](val global: T) {
  import global._
  import definitions._

  private var trackerStack: List[FlagTracker] = Nil
  private def trackerString = trackerStack.mkString(" ")

  class FlagTracker(val name: String) {
    private val mask               = symtab.Flags.TRANS_FLAG
    private val seen               = new mutable.HashSet[Symbol]

    private def debug(msg: String) = if (settings.debug.value) log(msg)
    private def trace(msg: String) = if (settings.debug.value && settings.verbose.value) log(msg)
    private def isDebug            = settings.debug.value
    private def doWeOwnFlag        = trackerStack.headOption exists (_ eq this)
    private def isOK               = trackerStack.isEmpty || (trackerStack.head eq this)

    def apply(sym: Symbol) = {
      if (!isOK)
        log("Testing " + sym.name + " for " + name + " flag, but not at top of stack: " + trackerString)

      sym hasFlag mask
    }
    def set(sym: Symbol) = {
      if (!isOK)
        log("Tried to set " + name + " but not at top of stack: " + trackerString)

      seen += sym
      sym setFlag mask
    }
    def reset(sym: Symbol) = {
      if (!isOK)
        log("Tried to clear " + name + " but not at top of stack: " + trackerString)

      seen -= sym
      sym resetFlag mask
    }
    def clear() {
      if (!doWeOwnFlag && seen.nonEmpty)
        log("Clearing " + seen.size + " " + name + " flags even though the stack is: " + trackerString)

      seen foreach (_ resetFlag mask)
      seen.clear()
    }
  }

  def forceClear() = {
    if (trackerStack.nonEmpty) {
      log("Warning: force clearing the stack at " + phase + ": " + trackerString)
      trackerStack foreach (_.clear())
      trackerStack = Nil
    }
  }

  def claimTransFlag(label: String): FlagTracker = {
    if (trackerStack.isEmpty || trackerStack.head.name != label)
      trackerStack ::= new FlagTracker(label)

    trackerStack.head
  }
  def releaseTransFlag(label: String): Boolean = {
    trackerStack.isEmpty || {
      if (trackerStack.head.name == label) {
        trackerStack.head.clear()
        trackerStack = trackerStack.tail
        true
      }
      else {
        log("Warning: trying to release " + label + " flag but the stack is: " + trackerStack.mkString(" "))
        false
      }
    }
  }
  def holdingTransFlag[U](label: String)(f: FlagTracker => U): U = {
    try {
      val t = claimTransFlag(label)
      f(t)
    }
    finally {
      releaseTransFlag(label)
    }
  }
}


/** Some functions for generating comments and methods involving flags,
 *  with the output determined by reflection so we can have a little more
 *  assurance that documentation and debugging output match up with reality.
 *
 *  For the compiler, the output can be generated with:
 *    scala scala.tools.nsc.util.FlagsUtilCompiler
 */
class FlagsUtil(flagsObject: AnyRef) {
  /** Override to tweak flag strings before output. */
  def addFlagMetadata(name: String) = name

  /** Runs the generative methods in this class. */
  def reflectiveAnalyzeFlags() = {
    mkFlagsTable()
    println("")
    mkFlagToStringMethod()
  }
  /** A list of the flag names found at each bit position.
   */
  def reflectiveFlagNames: List[List[String]] = {
    val pairs = flagMethods map { m =>
      m.getName -> ((m invoke flagsObject) match {
        case x: java.lang.Integer   => x.intValue: Long
        case x: java.lang.Long      => x.longValue
      })
    }
    (0 to 63).toList map { idx =>
      pairs collect { case (name, value) if value == (1L << idx) => name }
    }
  }
  /** Prints a comment table identifying all the flags (as seen
   *  via reflection) and at what bit each is located.
   */
  def mkFlagsTable() = {
    val markedFlagNames = reflectiveFlagNames map (_ map addFlagMetadata)

    val widths = 0 to 2 map { column =>
      markedFlagNames collect { case xs if xs.length > column =>
        xs(column).length
      } max
    }
    val fmt = "// %2d: " + (widths map ("%" + _ + "s") mkString " ")
    def padded(xs: List[String]) = xs match {
      case Nil              => List("", "", "")
      case x :: Nil         => List(x, "", "")
      case x1 :: x2 :: Nil  => List(x1, x2, "")
      case _                => xs take 3
    }
    println("// Generated by mkFlagsTable() at " + now + "\n//")
    // prints the grid showing which flags are at each index
    for ((names, idx) <- markedFlagNames.zipWithIndex) {
      println(fmt.format(idx :: padded(names) : _*))
    }
  }
  /** Prints an implementation of flagToString based on the reflectively
   *  determined contents of the flags class.
   */
  def mkFlagToStringMethod() = {
    def key(xs: List[String], flag: Long) = xs match {
      case Nil    => "%19s".format("0x" + "%x".format(flag) + "L")
      case x :: _ =>
        if (x.head.isLower) "`" + x + "`"
        else x
    }
    def value(xs: List[String], flag: Long) = "\"" + (xs match {
      case Nil                                      => ""
      case x :: Nil if (flag & ExplicitFlags) != 0  => x.toLowerCase
      case xs                                       => xs.map(_.toLowerCase).mkString("<", "/", ">")
    }) + "\""
    val pairs: List[(String, String)] = reflectiveFlagNames.zipWithIndex map {
      case (xs, idx) => (key(xs, 1L << idx), value(xs, 1L << idx))
    }
    val keyWidth  = pairs map (_._1.length) max
    val bodyWidth = pairs map (_._2.length) max
    val fmt       = "  case %" + keyWidth + "s => %-" + bodyWidth + "s // (1L << %d)"

    println("// Generated by mkFlagToStringMethod() at " + now)
    println("@annotation.switch override def flagToString(flag: Long): String = flag match {")
    for (((key, body), idx) <- pairs.zipWithIndex) {
      print(fmt.format(key, body, idx))
      println("")
    }
    println("  case _ => \"\"")
    println("}")
  }

  def isFlagName(s: String) = s stripPrefix "late" stripPrefix "not" forall (x => x.isUpper || x == '_')
  def flagMethods           = flagsObject.getClass.getMethods.toList filter (x => isFlagName(x.getName)) sortBy (_.getName)
  private def now           = new java.util.Date toString
}

object FlagsUtil {
  import reflect.generic.ModifierFlags

  trait MarkModifiers extends FlagsUtil {
    lazy val isModifiersFlag = classOf[ModifierFlags].getMethods map (_.getName) filter isFlagName toSet
    override def addFlagMetadata(name: String) = {
      if (isModifiersFlag(name)) name + "/M"
      else name
    }
  }
}

/** Convenience standalone programs.
 */
object FlagsUtilCompiler extends FlagsUtil(symtab.Flags) with FlagsUtil.MarkModifiers {
  def main(args: Array[String]): Unit = reflectiveAnalyzeFlags()
}

object FlagsUtilLibrary extends FlagsUtil(reflect.generic.Flags) with FlagsUtil.MarkModifiers {
  def main(args: Array[String]): Unit = reflectiveAnalyzeFlags()
}

