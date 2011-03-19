/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package interpreter

import scala.collection.{ mutable, immutable }
import scala.util.matching.Regex
import scala.tools.nsc.util.{ BatchSourceFile }
import session.{ History }

/** A class for methods to be injected into the intp in power mode.
 */
class Power(repl: ILoop, intp: IMain) {
  def this(repl: ILoop) = this(repl, repl.intp)
  def this(intp: IMain) = this(null, intp)

  val global: intp.global.type = intp.global
  import global._
  import intp.{ beQuietDuring, interpret, parse }

  abstract class SymSlurper {
    def isKeep(sym: Symbol): Boolean
    def isIgnore(sym: Symbol): Boolean
    def isRecur(sym: Symbol): Boolean
    def isFinished(): Boolean

    val keep = mutable.HashSet[Symbol]()
    val seen = mutable.HashSet[Symbol]()
    def processed = keep.size + seen.size
    def discarded = seen.size - keep.size

    def members(x: Symbol): List[Symbol] =
      if (x.rawInfo.isComplete) x.info.members
      else Nil

    var lastCount = -1
    var pass = 0
    val unseenHistory = new mutable.ListBuffer[Int]

    def loop(todo: Set[Symbol]): Set[Symbol] = {
      pass += 1
      val (repeats, unseen) = todo partition seen
      unseenHistory += unseen.size
      if (opt.verbose) {
        println("%3d  %s accumulated, %s discarded.  This pass: %s unseen, %s repeats".format(
          pass, keep.size, discarded, unseen.size, repeats.size))
      }
      if (lastCount == processed || unseen.isEmpty || isFinished())
        return keep.toSet

      lastCount = processed
      keep ++= (unseen filter isKeep filterNot isIgnore)
      seen ++= unseen
      loop(unseen filter isRecur flatMap members)
    }

    def apply(sym: Symbol): Set[Symbol] = {
      keep.clear()
      seen.clear()
      loop(Set(sym))
    }
  }

  class PackageSlurper(pkgName: String) extends SymSlurper {
    val pkgSymbol = getCompilerModule(pkgName)
    val modClass  = pkgSymbol.moduleClass

    /** Looking for dwindling returns */
    def droppedEnough() = unseenHistory.size >= 4 && (
      unseenHistory.takeRight(4).sliding(2) map (_.toList) forall {
        case List(a, b) => a > b
      }
    )

    def isRecur(sym: Symbol)  = true
    def isIgnore(sym: Symbol) = sym.isAnonOrRefinementClass || (sym.name.toString contains "$mc")
    def isKeep(sym: Symbol)   = sym.hasTransOwner(modClass)
    def isFinished()          = droppedEnough()
    def slurp()               = apply(modClass)
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
    |import power.phased
    |import power.Implicits._
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

  private def missingWrap(op: => Symbol): Symbol =
    try op
    catch { case _: MissingRequirementError => NoSymbol }

  private def getCompilerClass(name: String)  = missingWrap(definitions.getClass(name))
  private def getCompilerModule(name: String) = missingWrap(definitions.getModule(name))

  object InternalInfo {
    implicit def apply[T: Manifest] : InternalInfo[T] = new InternalInfo[T](None)
  }
  /** Todos...
   *    translate manifest type arguments into applied types
   *    customizable symbol filter (had to hardcode no-spec to reduce noise)
   */
  class InternalInfo[T: Manifest](value: Option[T] = None) {
    def companion = symbol.companionSymbol
    def info      = symbol.info
    def module    = symbol.moduleClass
    def owner     = symbol.owner
    def owners    = symbol.ownerChain drop 1
    def symDef    = symbol.defString
    def symName   = symbol.name
    def tpe       = symbol.tpe

    def declares  = members filter (_.owner == symbol)
    def inherits  = members filterNot (_.owner == symbol)
    def types     = members filter (_.name.isTypeName)
    def methods   = members filter (_.isMethod)
    def overrides = declares filter (_.isOverride)
    def inPackage = owners find (x => x.isPackageClass || x.isPackage) getOrElse definitions.RootPackage

    def erasure    = manifest[T].erasure
    def symbol     = getCompilerClass(erasure.getName)
    def members    = tpe.members filterNot (_.name.toString contains "$mc")
    def allMembers = tpe.members
    def bts        = info.baseTypeSeq.toList
    def btsmap     = bts map (x => (x, x.decls.toList)) toMap
    def pkgName    = erasure.getPackage.getName
    def pkg        = getCompilerModule(pkgName)
    def pkgmates   = pkg.tpe.members
    def pkgslurp   = new PackageSlurper(pkgName) slurp()

    def ?         = this

    def whoHas(name: String) = bts filter (_.decls.toList exists (_.name.toString == name))
    def <:<[U: Manifest](other: U) = tpe <:< InternalInfo[U].tpe
    def lub[U: Manifest](other: U) = global.lub(List(tpe, InternalInfo[U].tpe))
    def glb[U: Manifest](other: U) = global.glb(List(tpe, InternalInfo[U].tpe))

    def shortClass = erasure.getName split "[$.]" last
    override def toString = value match {
      case Some(x)  => "%s (%s)".format(x, shortClass)
      case _        => erasure.getName
    }
  }

  trait PCFormatter extends (Any => List[String]) {
    def apply(x: Any): List[String]

    private var indentLevel = 0
    private def spaces = "  " * indentLevel
    def indented[T](body: => T): T = {
      indentLevel += 1
      try body
      finally indentLevel -= 1
    }

    def show(x: Any): Unit = grep(x, _ => true)
    def grep(x: Any, p: String => Boolean): Unit =
      apply(x) filter p foreach (x => println(spaces + x))
  }
  class MultiPrintingConvenience[T](coll: Traversable[T])(implicit fmt: PCFormatter) {
    import fmt._

    def freqBy[U](p: T => U) = {
      val map = coll.toList groupBy p
      map.toList sortBy (-_._2.size)
    }
    def freqByFormatted[U](p: T => U) = {
      val buf = new mutable.ListBuffer[String]

      freqBy(p) foreach { case (k, vs) =>
        buf += "%d: %s".format(vs.size, k)
        vs flatMap fmt foreach (buf += "  " + _)
      }
      buf.toList
    }

    /** It makes sense.
     *
     *    #  means how many
     *    ?  means "I said, HOW MANY?"
     *    >  means print
     *
     *  Now don't you feel silly for what you were thinking.
     */
    def #?>[U](p: T => U) = this freqByFormatted p foreach println
    def #?[U](p: T => U) = this freqByFormatted p
  }

  class PrintingConvenience[T](value: T)(implicit fmt: PCFormatter) {
    def > : Unit = >(_ => true)
    def >(s: String): Unit = >(_ contains s)
    def >(r: Regex): Unit = >(_ matches r.pattern.toString)
    def >(p: String => Boolean): Unit = fmt.grep(value, p)
  }
  protected trait Implicits1 {
    implicit def replPrinting[T](x: T)(implicit fmt: PCFormatter) = new PrintingConvenience[T](x)
  }
  object Implicits extends Implicits1 {
    implicit lazy val powerNameOrdering: Ordering[Name]     = Ordering[String] on (_.toString)
    implicit lazy val powerSymbolOrdering: Ordering[Symbol] = Ordering[Name] on (_.name)
    implicit lazy val powerTypeOrdering: Ordering[Type]     = Ordering[Symbol] on (_.typeSymbol)

    object symbolSubtypeOrdering extends Ordering[Symbol] {
      def compare(s1: Symbol, s2: Symbol) =
        if (s1 eq s2) 0
        else if (s1 isLess s2) -1
        else 1
    }
    implicit def replCollPrinting[T](xs: Traversable[T])(implicit fmt: PCFormatter) = new MultiPrintingConvenience[T](xs)
    implicit def replInternalInfo[T: Manifest](x: T): InternalInfo[T] = new InternalInfo[T](Some(x))
    implicit object ReplDefaultFormatter extends PCFormatter {
      def apply(x: Any): List[String] = x match {
        case Tuple2(k, v)       => List(apply(k) ++ Seq("->") ++ apply(v) mkString " ")
        case xs: Traversable[_] => (xs.toList flatMap apply).sorted.distinct
        case x                  => List("" + x)
      }
    }
  }

  object phased extends Phased {
    val global: Power.this.global.type = Power.this.global
  }
  def context(code: String)           = analyzer.rootContext(unit(code))
  def source(code: String)            = new BatchSourceFile("<console>", code)
  def unit(code: String)              = new CompilationUnit(source(code))
  def trees(code: String): List[Tree] = parse(code) getOrElse Nil
  def typeOf(id: String): Type        = intp.typeOfExpression(id) getOrElse NoType

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
