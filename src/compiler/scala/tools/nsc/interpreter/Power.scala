/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package interpreter

import scala.reflect.AnyValManifest
import scala.collection.{ mutable, immutable }
import scala.util.matching.Regex
import scala.tools.nsc.util.{ BatchSourceFile }
import session.{ History }
import scala.io.Codec
import java.net.{ URL, MalformedURLException }
import io.{ Path }

trait SharesGlobal {
  type GlobalType <: Global
  val global: GlobalType

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
  def apply(intp: IMain): Power = apply(null, intp)
  def apply(repl: ILoop): Power = apply(repl, repl.intp)
  def apply(repl: ILoop, intp: IMain): Power =
    new Power(repl, intp) {
      type GlobalType = intp.global.type
      final val global: intp.global.type = intp.global
    }
}

/** A class for methods to be injected into the intp in power mode.
 */
abstract class Power(
  val repl: ILoop,
  val intp: IMain
) extends SharesGlobal {
  import intp.{
    beQuietDuring, typeOfExpression, getCompilerClass, getCompilerModule,
    interpret, parse
  }
  import global._

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
    def droppedEnough() = unseenHistory.size >= 4 && {
      unseenHistory takeRight 4 sliding 2 forall { it =>
        val List(a, b) = it.toList
        a > b
      }
    }

    def isRecur(sym: Symbol)  = true
    def isIgnore(sym: Symbol) = sym.isAnonOrRefinementClass || (sym.name.toString contains "$mc")
    def isKeep(sym: Symbol)   = sym.hasTransOwner(modClass)
    def isFinished()          = droppedEnough()
    def slurp()               = apply(modClass)
  }

  private def customBanner = replProps.powerBanner.option flatMap (f => io.File(f).safeSlurp())
  private def customInit   = replProps.powerInitCode.option flatMap (f => io.File(f).safeSlurp())

  def banner = customBanner getOrElse """
    |** Power User mode enabled - BEEP BOOP SPIZ **
    |** :phase has been set to 'typer'.          **
    |** scala.tools.nsc._ has been imported      **
    |** global._ and definitions._ also imported **
    |** Try  :help,  vals.<tab>,  power.<tab>    **
  """.stripMargin.trim

  private def initImports = List(
    "scala.tools.nsc._",
    "scala.collection.JavaConverters._",
    "intp.global.{ error => _, _ }",
    "definitions.{ getClass => _, _ }",
    "power.Implicits._",
    "power.rutil._"
  )

  def init = customInit match {
    case Some(x)  => x
    case _        => initImports.mkString("import ", ", ", "")
  }

  /** Starts up power mode and runs whatever is in init.
   */
  def unleash(): Unit = beQuietDuring {
    // First we create the ReplVals instance and bind it to $r
    intp.bind("$r", new ReplVals(repl))
    // Then we import everything from $r.
    intp interpret ("import " + intp.pathToTerm("$r") + "._")
    // And whatever else there is to do.
    init.lines foreach (intp interpret _)
  }

  trait LowPriorityInternalInfo {
    implicit def apply[T: Manifest] : InternalInfo[T] = new InternalInfo[T](None)
  }
  object InternalInfo extends LowPriorityInternalInfo { }

  /** Todos...
   *    translate manifest type arguments into applied types
   *    customizable symbol filter (had to hardcode no-spec to reduce noise)
   */
  class InternalInfo[T: Manifest](value: Option[T] = None) {
    // Decided it was unwise to have implicit conversions via commonly
    // used type/symbol methods, because it's too easy to e.g. call
    // "x.tpe" where x is a Type, and rather than failing you get the
    // Type representing Types#Type (or Manifest, or whatever.)
    private def tpe    = tpe_
    private def symbol = symbol_
    private def name   = name_

    // Would love to have stuff like existential types working,
    // but very unfortunately those manifests just stuff the relevant
    // information into the toString method.  Boo.
    private def manifestToType(m: Manifest[_]): Type = m match {
      case x: AnyValManifest[_] =>
        getCompilerClass("scala." + x).tpe
      case _                             =>
        val name = m.erasure.getName
        if (name endsWith nme.MODULE_SUFFIX_STRING) getCompilerModule(name dropRight 1).tpe
        else {
          val sym  = getCompilerClass(name)
          val args = m.typeArguments

          if (args.isEmpty) sym.tpe
          else typeRef(NoPrefix, sym, args map manifestToType)
        }
    }

    def symbol_ : Symbol = getCompilerClass(erasure.getName)
    def tpe_ : Type      = manifestToType(man)
    def name_ : Name     = symbol.name
    def companion        = symbol.companionSymbol
    def info             = symbol.info
    def module           = symbol.moduleClass
    def owner            = symbol.owner
    def owners           = symbol.ownerChain drop 1
    def defn             = symbol.defString

    def declares  = members filter (_.owner == symbol)
    def inherits  = members filterNot (_.owner == symbol)
    def types     = members filter (_.name.isTypeName)
    def methods   = members filter (_.isMethod)
    def overrides = declares filter (_.isOverride)
    def inPackage = owners find (x => x.isPackageClass || x.isPackage) getOrElse definitions.RootPackage

    def man        = manifest[T]
    def erasure    = man.erasure
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

    def whoHas(name: String) = bts filter (_.decls exists (_.name.toString == name))
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
      def show(x: Any): Unit = prettify(x) foreach println
      def prettify(x: Any): TraversableOnce[String] = x match {
        case x: Name                => List(x.decode)
        case Tuple2(k, v)           => List(prettify(k).toIterator ++ Iterator("->") ++ prettify(v) mkString " ")
        case xs: Array[_]           => xs.iterator flatMap prettify
        case xs: TraversableOnce[_] => xs flatMap prettify
        case x                      => List(Prettifier.stringOf(x))
      }
    }
  }
  object StringPrettifier extends Prettifier[String] {
    def show(x: String) = println(x)
    def prettify(x: String) = List(Prettifier stringOf x)
  }
  object Prettifier extends LowPriorityPrettifier {
    def stringOf(x: Any): String = scala.runtime.ScalaRunTime.stringOf(x)
    def prettify[T](value: T): TraversableOnce[String] = default[T] prettify value
    def default[T] = new Prettifier[T] {
      def prettify(x: T): TraversableOnce[String] = AnyPrettifier prettify x
      def show(x: T): Unit = AnyPrettifier show x
    }
  }
  trait Prettifier[T] {
    def show(x: T): Unit
    def prettify(x: T): TraversableOnce[String]

    def show(xs: TraversableOnce[T]): Unit = prettify(xs) foreach println
    def prettify(xs: TraversableOnce[T]): TraversableOnce[String] = xs flatMap (x => prettify(x))
  }

  abstract class PrettifierClass[T: Prettifier]() {
    val pretty = implicitly[Prettifier[T]]
    import pretty._

    def value: Seq[T]

    def pp(f: Seq[T] => Seq[T]): Unit =
      pretty prettify f(value) foreach (StringPrettifier show _)

    def freq[U](p: T => U) = (value.toSeq groupBy p mapValues (_.size)).toList sortBy (-_._2) map (_.swap)
    def ppfreq[U](p: T => U): Unit = freq(p) foreach { case (count, key) => println("%5d %s".format(count, key)) }

    def |[U](f: Seq[T] => Seq[U]): Seq[U]        = f(value)
    def ^^[U](f: T => U): Seq[U]                 = value map f
    def ^?[U](pf: PartialFunction[T, U]): Seq[U] = value collect pf

    def >>!(implicit ord: Ordering[T]): Unit     = pp(_.sorted.distinct)
    def >>(implicit ord: Ordering[T]): Unit      = pp(_.sorted)
    def >!(): Unit                               = pp(_.distinct)
    def >(): Unit                                = pp(identity)

    def >#(): Unit                               = this ># (identity[T] _)
    def >#[U](p: T => U): Unit                   = this ppfreq p

    def >?(p: T => Boolean): Unit                = pp(_ filter p)
    def >?(s: String): Unit                      = pp(_ filter (_.toString contains s))
    def >?(r: Regex): Unit                       = pp(_ filter (_.toString matches fixRegex(r)))

    private def fixRegex(r: scala.util.matching.Regex): String = {
      val s = r.pattern.toString
      val prefix = if (s startsWith "^") "" else """^.*?"""
      val suffix = if (s endsWith "$") "" else """.*$"""

      prefix + s + suffix
    }
  }

  class MultiPrettifierClass[T: Prettifier](val value: Seq[T]) extends PrettifierClass[T]() { }
  class SinglePrettifierClass[T: Prettifier](single: T) extends PrettifierClass[T]() {
    val value = List(single)
  }

  class RichReplString(s: String) {
    // pretty print the string
    def pp() { intp.prettyPrint(s) }
    // make an url out of the string
    def u: URL = (
      if (s contains ":") new URL(s)
      else if (new JFile(s) exists) new JFile(s).toURI.toURL
      else new URL("http://" + s)
    )
  }
  class RichInputStream(in: InputStream)(implicit codec: Codec) {
    def bytes(): Array[Byte]  = io.Streamable.bytes(in)
    def slurp(): String       = io.Streamable.slurp(in)
    def <<(): String          = slurp()
  }
  class RichReplURL(url: URL)(implicit codec: Codec) {
    def slurp(): String = io.Streamable.slurp(url)
    def pp() { intp prettyPrint slurp() }
  }

  protected trait Implicits1 {
    // fallback
    implicit def replPrinting[T](x: T)(implicit pretty: Prettifier[T] = Prettifier.default[T]) =
      new SinglePrettifierClass[T](x)
  }
  trait Implicits2 extends Implicits1 {
    class RichSymbol(sym: Symbol) {
      // convenient type application
      def apply(targs: Type*): Type = typeRef(NoPrefix, sym, targs.toList)
    }
    object symbolSubtypeOrdering extends Ordering[Symbol] {
      def compare(s1: Symbol, s2: Symbol) =
        if (s1 eq s2) 0
        else if (s1 isLess s2) -1
        else 1
    }
    implicit lazy val powerNameOrdering: Ordering[Name]     = Ordering[String] on (_.toString)
    implicit lazy val powerSymbolOrdering: Ordering[Symbol] = Ordering[Name] on (_.name)
    implicit lazy val powerTypeOrdering: Ordering[Type]     = Ordering[Symbol] on (_.typeSymbol)

    implicit def replInternalInfo[T: Manifest](x: T): InternalInfo[T] = new InternalInfo[T](Some(x))
    implicit def replEnhancedStrings(s: String): RichReplString = new RichReplString(s)
    implicit def replMultiPrinting[T: Prettifier](xs: TraversableOnce[T]): MultiPrettifierClass[T] =
      new MultiPrettifierClass[T](xs.toSeq)
    implicit def replPrettifier[T] : Prettifier[T] = Prettifier.default[T]
    implicit def replTypeApplication(sym: Symbol): RichSymbol = new RichSymbol(sym)

    implicit def replInputStream(in: InputStream)(implicit codec: Codec) = new RichInputStream(in)
    implicit def replEnhancedURLs(url: URL)(implicit codec: Codec): RichReplURL = new RichReplURL(url)(codec)
  }
  object Implicits extends Implicits2 { }

  trait ReplUtilities {
    def module[T: Manifest] = getCompilerModule(manifest[T].erasure.getName stripSuffix nme.MODULE_SUFFIX_STRING)
    def clazz[T: Manifest] = getCompilerClass(manifest[T].erasure.getName)
    def info[T: Manifest] = InternalInfo[T]
    def ?[T: Manifest] = InternalInfo[T]
    def url(s: String) = {
      try new URL(s)
      catch { case _: MalformedURLException =>
        if (Path(s).exists) Path(s).toURL
        else new URL("http://" + s)
      }
    }
    def sanitize(s: String): String = sanitize(s.getBytes())
    def sanitize(s: Array[Byte]): String = (s map {
      case x if x.toChar.isControl  => '?'
      case x                        => x.toChar
    }).mkString

    def strings(s: Seq[Byte]): List[String] = {
      if (s.length == 0) Nil
      else s dropWhile (_.toChar.isControl) span (x => !x.toChar.isControl) match {
        case (next, rest) => next.map(_.toChar).mkString :: strings(rest)
      }
    }
  }

  lazy val rutil: ReplUtilities = new ReplUtilities { }

  lazy val phased: Phased = new Phased with SharesGlobal {
    type GlobalType = Power.this.global.type
    final val global: Power.this.global.type = Power.this.global
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
