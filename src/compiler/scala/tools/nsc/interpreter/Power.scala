/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package interpreter

import scala.reflect.NameTransformer
import scala.collection.{ mutable, immutable }
import scala.util.matching.Regex
import scala.tools.nsc.util.{ BatchSourceFile }
import session.{ History }

trait SharesGlobal[G <: Global] {
  val global: G

  // This business gets really old:
  //
  // found   : power.intp.global.Symbol
  // required: global.Symbol
  //
  // Have tried many ways to cast it aside, this is the current winner.
  // Todo: figure out a way to abstract over all the type members.
  type AnySymbol   = Global#Symbol
  type AnyType     = Global#Type
  type AnyName     = Global#Name
  type AnyTree     = Global#Tree

  type Symbol   = global.Symbol
  type Type     = global.Type
  type Name     = global.Name
  type Tree     = global.Tree

  implicit def upDependentSymbol(x: AnySymbol): Symbol = x.asInstanceOf[Symbol]
  implicit def upDependentType(x: AnyType): Type = x.asInstanceOf[Type]
  implicit def upDependentName(x: AnyName): Name = x.asInstanceOf[Name]
  implicit def upDependentTree(x: AnyTree): Tree = x.asInstanceOf[Tree]
}

object Power {
  def apply[G <: Global](repl: ILoop, g: G) =
    new { final val global: G = g }
      with Power[G](repl, repl.intp) // .asInstanceOf[IMain { val global: G }])

  def apply(intp: IMain) =
    new { final val global = intp.global }
      with Power[Global](null, intp)
}

/** A class for methods to be injected into the intp in power mode.
 */
abstract class Power[G <: Global](
  val repl: ILoop,
  val intp: IMain   // { val global: G }
) extends SharesGlobal[G] {
  import intp.{ beQuietDuring, interpret, parse }
  import global.{ opt, definitions, stringToTermName, NoSymbol, NoType, analyzer, CompilationUnit }

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
    |import interpreter.Power
    |final val global = repl.power.global
    |final val power = repl.power.asInstanceOf[Power[global.type]]
    |final val intp = repl.intp
    |import global._
    |import definitions._
    |import power.phased
    |import power.Implicits.{ global => _, _ }
  """.stripMargin

  /** Starts up power mode and runs whatever is in init.
   */
  def unleash(): Unit = beQuietDuring {
    intp.bind[ILoop]("repl", repl)
    intp.bind[History]("history", repl.in.history)
    intp.bind("completion", repl.in.completion)
    intp.bind[ISettings]("isettings", intp.isettings)
    init split '\n' foreach interpret
  }

  private def missingWrap(op: => Symbol): Symbol =
    try op
    catch { case _: MissingRequirementError => NoSymbol }

  private def getCompilerClass(name: String)  = missingWrap(definitions.getClass(name))
  private def getCompilerModule(name: String) = missingWrap(definitions.getModule(name))

  trait LowPriorityInternalInfo {
    implicit def apply[T: Manifest] : InternalInfo[T] = new InternalInfo[T](None)
  }
  object InternalInfo extends LowPriorityInternalInfo { }

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
    def pkgName    = Option(erasure.getPackage) map (_.getName)
    def pkg        = pkgName map getCompilerModule getOrElse NoSymbol
    def pkgmates   = pkg.tpe.members
    def pkgslurp   = pkgName match {
      case Some(name) => new PackageSlurper(name) slurp()
      case _          => Set()
    }
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

  trait LowPriorityPrettifier {
    implicit object AnyPrettifier extends Prettifier[Any] {
      def prettify(x: Any): List[String] = x match {
        case x: Name            => List(x.decode)
        case Tuple2(k, v)       => List(prettify(k) ++ Seq("->") ++ prettify(v) mkString " ")
        case xs: Traversable[_] => (xs.toList flatMap prettify).sorted.distinct
        case x                  => List("" + x)
      }
    }
  }
  object Prettifier extends LowPriorityPrettifier {
    def prettify[T](value: T): List[String] = default[T] prettify value
    def default[T] = new Prettifier[T] {
      def prettify(x: T): List[String] = AnyPrettifier prettify x
    }
  }
  trait Prettifier[T] {
    def prettify(x: T): List[String]

    private var indentLevel = 0
    private def spaces = "  " * indentLevel
    def indented[T](body: => T): T = {
      indentLevel += 1
      try body
      finally indentLevel -= 1
    }

    def show(x: T): Unit = grep(x, _ => true)
    def grep(x: T, p: String => Boolean): Unit =
      prettify(x) filter p foreach (x => println(spaces + x))
  }
  class MultiPrintingConvenience[T: Prettifier](coll: Traversable[T]) {
    val pretty = implicitly[Prettifier[T]]
    import pretty._

    def freqBy[U](p: T => U) = {
      val map = coll.toList groupBy p
      map.toList sortBy (-_._2.size)
    }
    def freqByFormatted[U](p: T => U) = {
      val buf = new mutable.ListBuffer[String]

      freqBy(p) foreach { case (k, vs) =>
        buf += "%d: %s".format(vs.size, Prettifier.prettify(k))
        vs flatMap prettify foreach (buf += "  " + _)
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

  class PrintingConvenience[T: Prettifier](value: T) {
    val pretty = implicitly[Prettifier[T]]

    def > { >(_ => true) }
    def >(s: String): Unit = >(_ contains s)
    def >(r: Regex): Unit = >(_ matches r.pattern.toString)
    def >(p: String => Boolean): Unit = pretty.grep(value, p)
  }
  protected trait Implicits1 {
    // fallback
    implicit def replPrinting[T](x: T)(implicit pretty: Prettifier[T] = Prettifier.default[T]) = new PrintingConvenience[T](x)
  }
  object Implicits extends Implicits1 with SharesGlobal[G] {
    val global = Power.this.global
    import global._

    implicit lazy val powerNameOrdering: Ordering[Name]     = Ordering[String] on (_.toString)
    implicit lazy val powerSymbolOrdering: Ordering[Symbol] = Ordering[Name] on (_.name)
    implicit lazy val powerTypeOrdering: Ordering[Type]     = Ordering[Symbol] on (_.typeSymbol)

    object symbolSubtypeOrdering extends Ordering[Symbol] {
      def compare(s1: Symbol, s2: Symbol) =
        if (s1 eq s2) 0
        else if (s1 isLess s2) -1
        else 1
    }
    implicit def replCollPrinting[T: Prettifier](xs: Traversable[T]): MultiPrintingConvenience[T] = new MultiPrintingConvenience[T](xs)
    implicit def replInternalInfo[T: Manifest](x: T): InternalInfo[T] = new InternalInfo[T](Some(x))
    implicit def replPrettifier[T] : Prettifier[T] = Prettifier.default[T]
    implicit def vararsTypeApplication(sym: Symbol) = new {
      def apply(targs: Type*) = typeRef(NoPrefix, sym, targs.toList)
    }
    def ?[T: Manifest] = InternalInfo[T]
  }

  object phased extends Phased with SharesGlobal[G] {
    val global: G = Power.this.global
  }
  def context(code: String)    = analyzer.rootContext(unit(code))
  def source(code: String)     = new BatchSourceFile("<console>", code)
  def unit(code: String)       = new CompilationUnit(source(code))
  def trees(code: String)      = parse(code) getOrElse Nil
  def typeOf(id: String): Type = intp.typeOfExpression(id) getOrElse NoType

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
