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
import scala.io.Codec
import java.net.{ URL, MalformedURLException }
import io.{ Path }
import language.implicitConversions

/** Collecting some power mode examples.

scala> trait F[@specialized(Int) T] { def f: T = ??? }
defined trait F

scala> trait G[@specialized(Long, Int) T] extends F[T] { override def f: T = super.f }
defined trait G

scala> changesAfterEachPhase(intp("G").info.members filter (_.name.toString contains "super")) >
Gained after  1/parser {
  method super$f
}

Gained after 12/specialize {
  method super$f$mcJ$sp
  method super$f$mcI$sp
}

Lost after 18/flatten {
  method super$f$mcJ$sp
  method super$f$mcI$sp
  method super$f
}
*/

/** A class for methods to be injected into the intp in power mode.
 */
class Power[ReplValsImpl <: ReplVals : TypeTag](val intp: IMain, replVals: ReplValsImpl) {
  import intp.{ beQuietDuring, typeOfExpression, interpret, parse }
  import intp.global._
  import definitions.{ compilerTypeFromTag, compilerSymbolFromTag, getClassIfDefined, getModuleIfDefined }

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

  class PackageSlurper(packageClass: Symbol) extends SymSlurper {
    /** Looking for dwindling returns */
    def droppedEnough() = unseenHistory.size >= 4 && {
      unseenHistory takeRight 4 sliding 2 forall { it =>
        val List(a, b) = it.toList
        a > b
      }
    }

    def isRecur(sym: Symbol)  = true
    def isIgnore(sym: Symbol) = sym.isAnonOrRefinementClass || (sym.name.toString contains "$mc")
    def isKeep(sym: Symbol)   = sym.hasTransOwner(packageClass)
    def isFinished()          = droppedEnough()
    def slurp()               = {
      if (packageClass.isPackageClass)
        apply(packageClass)
      else {
        repldbg("Not a package class! " + packageClass)
        Set()
      }
    }
  }

  private def customBanner = replProps.powerBanner.option flatMap (f => io.File(f).safeSlurp())
  private def customInit   = replProps.powerInitCode.option flatMap (f => io.File(f).safeSlurp())

  def banner = customBanner getOrElse """
    |** Power User mode enabled - BEEP WHIR GYVE **
    |** :phase has been set to 'typer'.          **
    |** scala.tools.nsc._ has been imported      **
    |** global._, definitions._ also imported    **
    |** Try  :help, :vals, power.<tab>           **
  """.stripMargin.trim

  private def initImports = List(
    "scala.tools.nsc._",
    "scala.collection.JavaConverters._",
    "intp.global.{ error => _, _ }",
    "definitions.{ getClass => _, _ }",
    "power.rutil._",
    "replImplicits._",
    "treedsl.CODE._"
  )

  def init = customInit match {
    case Some(x)  => x
    case _        => initImports.mkString("import ", ", ", "")
  }

  /** Starts up power mode and runs whatever is in init.
   */
  def unleash(): Unit = beQuietDuring {
    // First we create the ReplVals instance and bind it to $r
    intp.bind("$r", replVals)
    // Then we import everything from $r.
    intp interpret ("import " + intp.pathToTerm("$r") + "._")
    // And whatever else there is to do.
    init.lines foreach (intp interpret _)
  }
  def valsDescription: String = {
    def to_str(m: Symbol) = "%12s %s".format(
      m.decodedName, "" + elimRefinement(m.accessedOrSelf.tpe) stripPrefix "scala.tools.nsc.")

    ( rutil.info[ReplValsImpl].membersDeclared
        filter (m => m.isPublic && !m.hasModuleFlag && !m.isConstructor)
        sortBy (_.decodedName)
           map to_str
      mkString ("Name and type of values imported into the repl in power mode.\n\n", "\n", "")
    )
  }

  trait LowPriorityInternalInfo {
    implicit def apply[T: TypeTag] : InternalInfo[T] = new InternalInfo[T](None)
  }
  object InternalInfo extends LowPriorityInternalInfo { }

  /** Now dealing with the problem of acidentally calling a method on Type
   *  when you're holding a Symbol and seeing the Symbol converted to the
   *  type of Symbol rather than the type of the thing represented by the
   *  symbol, by only implicitly installing one method, "?", and the rest
   *  of the conveniences exist on that wrapper.
   */
  trait LowPriorityInternalInfoWrapper {
    implicit def apply[T: TypeTag] : InternalInfoWrapper[T] = new InternalInfoWrapper[T](None)
  }
  object InternalInfoWrapper extends LowPriorityInternalInfoWrapper {

  }
  class InternalInfoWrapper[T: TypeTag](value: Option[T] = None) {
    def ? : InternalInfo[T] = new InternalInfo[T](value)
  }

  /** Todos...
   *    translate tag type arguments into applied types
   *    customizable symbol filter (had to hardcode no-spec to reduce noise)
   */
  class InternalInfo[T: TypeTag](value: Option[T] = None) {
    private def newInfo[U: TypeTag](value: U): InternalInfo[U] = new InternalInfo[U](Some(value))
    private def isSpecialized(s: Symbol) = s.name.toString contains "$mc"
    private def isImplClass(s: Symbol)   = s.name.toString endsWith "$class"

    /** Standard noise reduction filter. */
    def excludeMember(s: Symbol) = (
         isSpecialized(s)
      || isImplClass(s)
      || s.isAnonOrRefinementClass
      || s.isAnonymousFunction
    )
    def symbol      = compilerSymbolFromTag(tag)
    def tpe         = compilerTypeFromTag(tag)
    def name        = symbol.name
    def companion   = symbol.companionSymbol
    def info        = symbol.info
    def moduleClass = symbol.moduleClass
    def owner       = symbol.owner
    def owners      = symbol.ownerChain drop 1
    def signature   = symbol.defString

    def decls         = info.decls
    def declsOverride = membersDeclared filter (_.isOverride)
    def declsOriginal = membersDeclared filterNot (_.isOverride)

    def members           = membersUnabridged filterNot excludeMember
    def membersUnabridged = tpe.members
    def membersDeclared   = members filterNot excludeMember
    def membersInherited  = members filterNot (membersDeclared contains _)
    def memberTypes       = members filter (_.name.isTypeName)
    def memberMethods     = members filter (_.isMethod)

    def pkg             = symbol.enclosingPackage
    def pkgName         = pkg.fullName
    def pkgClass        = symbol.enclosingPackageClass
    def pkgMembers      = pkg.info.members filterNot excludeMember
    def pkgClasses      = pkgMembers filter (s => s.isClass && s.isDefinedInPackage)
    def pkgSymbols      = new PackageSlurper(pkgClass).slurp() filterNot excludeMember

    def tag            = typeTag[T]
    def erasure        = tag.erasure
    def shortClass     = erasure.getName split "[$.]" last

    def baseClasses                    = tpe.baseClasses
    def baseClassDecls                 = mapFrom(baseClasses)(_.info.decls.toList.sortBy(_.name))
    def ancestors                      = baseClasses drop 1
    def ancestorDeclares(name: String) = ancestors filter (_.info member newTermName(name) ne NoSymbol)
    def baseTypes                      = tpe.baseTypeSeq.toList

    def <:<[U: TypeTag](other: U) = tpe <:< newInfo(other).tpe
    def lub[U: TypeTag](other: U) = intp.global.lub(List(tpe, newInfo(other).tpe))
    def glb[U: TypeTag](other: U) = intp.global.glb(List(tpe, newInfo(other).tpe))

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
  }
  class RichSymbolList(syms: List[Symbol]) {
    def sigs  = syms map (_.defString)
    def infos = syms map (_.info)
  }

  trait Implicits1 {
    // fallback
    implicit def replPrinting[T](x: T)(implicit pretty: Prettifier[T] = Prettifier.default[T]) =
      new SinglePrettifierClass[T](x)

    implicit def liftToTypeName(s: String): TypeName = newTypeName(s)
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
    implicit lazy val powerSymbolOrdering: Ordering[Symbol] = Ordering[Name] on (_.name)
    implicit lazy val powerTypeOrdering: Ordering[Type]     = Ordering[Symbol] on (_.typeSymbol)

    implicit def replInternalInfo[T: TypeTag](x: T): InternalInfoWrapper[T] = new InternalInfoWrapper[T](Some(x))
    implicit def replEnhancedStrings(s: String): RichReplString = new RichReplString(s)
    implicit def replMultiPrinting[T: Prettifier](xs: TraversableOnce[T]): MultiPrettifierClass[T] =
      new MultiPrettifierClass[T](xs.toSeq)
    implicit def replPrettifier[T] : Prettifier[T] = Prettifier.default[T]
    implicit def replTypeApplication(sym: Symbol): RichSymbol = new RichSymbol(sym)

    implicit def replInputStream(in: InputStream)(implicit codec: Codec) = new RichInputStream(in)
    implicit def replEnhancedURLs(url: URL)(implicit codec: Codec): RichReplURL = new RichReplURL(url)(codec)

    implicit def liftToTermName(s: String): TermName = newTermName(s)
    implicit def replListOfSymbols(xs: List[Symbol]) = new RichSymbolList(xs)
  }

  trait ReplUtilities {
    // [Eugene to Paul] needs review!
    // def module[T: TypeTag] = getModuleIfDefined(typeTag[T].erasure.getName stripSuffix nme.MODULE_SUFFIX_STRING)
    // def clazz[T: TypeTag] = getClassIfDefined(typeTag[T].erasure.getName)
    def module[T: TypeTag] = typeTag[T].sym.suchThat(_.isPackage)
    def clazz[T: TypeTag] = typeTag[T].sym.suchThat(_.isClass)
    def info[T: TypeTag] = InternalInfo[T]
    def ?[T: TypeTag] = InternalInfo[T]
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
  lazy val phased: Phased       = new { val global: intp.global.type = intp.global } with Phased { }

  def context(code: String)    = analyzer.rootContext(unit(code))
  def source(code: String)     = newSourceFile(code)
  def unit(code: String)       = newCompilationUnit(code)
  def trees(code: String)      = parse(code) getOrElse Nil
  def typeOf(id: String)       = intp.typeOfExpression(id)

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
