/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package interpreter

import scala.collection.{ mutable, immutable }
import scala.tools.nsc.util.{ BatchSourceFile }

/** A class for methods to be injected into the intp in power mode.
 */
class Power(repl: ILoop, intp: IMain) {
  def this(repl: ILoop) = this(repl, repl.intp)
  def this(intp: IMain) = this(null, intp)

  val global: intp.global.type = intp.global

  import global._
  import definitions.{ getMember, getModule, getClass => getCompilerClass }
  import intp.{ beQuietDuring, interpret, parse }

  object phased extends Phased {
    val global: Power.this.global.type = Power.this.global
  }

  class ReplSnippet[T](val path: String, initial: T) {
    var code: String = ""
    var value: T = initial

    def set(code: String) = interpret(path + ".value = " + code)
    def get: T = value
    override def toString = "intp." + path + ".value = \"" + code + "\""
  }

  object vars {
    private def create[T](name: String, initial: T): ReplSnippet[T] =
      new ReplSnippet[T]("power.vars." + name, initial)

    val symfilter = create("symfilter", (s: Symbol) => true)
  }

  def banner = """
    |** Power User mode enabled - BEEP BOOP WHIR **
    |** scala.tools.nsc._ has been imported      **
    |** global._ and definitions._ also imported **
    |** New vals! Try repl, intp, global, power  **
    |** New cmds! :help to discover them         **
    |** New defs! Type power.<tab> to reveal     **
  """.stripMargin.trim

  def init = """
    |import scala.tools.nsc._
    |val global: intp.global.type = intp.global
    |import global._
    |import definitions._
    |import power.{ phased, show, clazz, module }
  """.stripMargin

  /** Starts up power mode and runs whatever is in init.
   */
  def unleash(): Unit = beQuietDuring {
    if (repl != null) {
      intp.bind[ILoop]("repl", repl)
      intp.bind[History]("history", repl.in.history)
      intp.bind("completion", repl.in.completion)
    }

    intp.bind[IMain]("intp", intp)
    intp.bind[Power]("power", this)
    intp.bind[ISettings]("isettings", intp.isettings)
    init split '\n' foreach interpret
  }

  object show {
    private def defStrings(sym: Symbol, p: Symbol => Boolean) =
      phased(sym.info.members filter p map (_.defString))

    private def display(sym: Symbol, p: Symbol => Boolean) =
      defStrings(sym, p) foreach println

    def methods[T: Manifest] = display(clazz[T], _.isMethod)
    def apply[T: Manifest] = display(clazz[T], vars.symfilter.get)
  }

  abstract class NameBased[T <: Name] {
    def mkName(s: String): T
    def mkSymbol(s: String): Symbol

    def apply[T: Manifest]                 = mkSymbol(manifest[T].erasure.getName)
    def tpe[T: Manifest]                   = apply[T].tpe
    def members[T: Manifest]               = tpe[T].members
    def member[T: Manifest](name: Name)    = getMember(apply[T], name)
    def vmember[T: Manifest](name: String) = member[T](newTermName(name))
    def tmember[T: Manifest](name: String) = member[T](newTypeName(name))
  }
  private def missingWrap(op: => Symbol): Symbol =
    try op
    catch { case _: MissingRequirementError => NoSymbol }

  object clazz extends NameBased[TypeName] {
    def mkName(s: String) = newTypeName(s)
    def mkSymbol(s: String): Symbol = missingWrap(getCompilerClass(s))
  }
  object module extends NameBased[TermName] {
    def mkName(s: String) = newTermName(s)
    def mkSymbol(s: String): Symbol = missingWrap(getModule(s))
  }

  def mkContext(code: String = "") = analyzer.rootContext(mkUnit(code))
  def mkAlias(name: String, what: String) = interpret("type %s = %s".format(name, what))
  def mkSourceFile(code: String) = new BatchSourceFile("<console>", code)
  def mkUnit(code: String) = new CompilationUnit(mkSourceFile(code))

  def mkTree(code: String): Tree = mkTrees(code).headOption getOrElse EmptyTree
  def mkTrees(code: String): List[Tree] = parse(code) getOrElse Nil
  def mkTypedTrees(code: String*): List[Tree] = {
    class TyperRun extends Run {
      override def stopPhase(name: String) = name == "superaccessors"
    }

    reporter.reset
    val run = new TyperRun
    run compileSources (code.toList.zipWithIndex map {
      case (s, i) => new BatchSourceFile("<console %d>".format(i), s)
    })
    run.units.toList map (_.body)
  }
  def mkTypedTree(code: String) = mkTypedTrees(code).head
  def mkType(id: String): Type = intp.typeOfExpression(id) getOrElse NoType

  override def toString = """
    |** Power mode status **
    |Default phase: %s
    |Names: %s
    |Identifiers: %s
  """.stripMargin.format(
      phased.get,
      intp.allDefinedNames mkString " ",
      intp.unqualifiedIds mkString " "
    )
}
