/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.interpreter

import java.io.InputStream
import java.net.{URI, URL}

import scala.collection.mutable
import scala.io.Codec
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.reflect.runtime.{universe => ru}
import scala.tools.nsc.io

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
class Power[ReplValsImpl <: ReplVals : ru.TypeTag: ClassTag](val intp: IMain, replVals: ReplValsImpl) {
  import intp.global._

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
      if (x.rawInfo.isComplete) x.info.members.toList
      else Nil

    var lastCount = -1
    var pass = 0
    val unseenHistory = new mutable.ListBuffer[Int]

    def loop(todo: Set[Symbol]): Set[Symbol] = {
      pass += 1
      val (repeats, unseen) = todo partition seen
      unseenHistory += unseen.size
      if (settings.verbose.value) {
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
        val List(a, b) = (it.toList: @unchecked)
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
//        repldbg("Not a package class! " + packageClass)
        Set()
      }
    }
  }


  def classic = """
    |** Power User mode enabled - BEEP WHIR GYVE **
    |** :phase has been set to 'typer'.          **
    |** scala.tools.nsc._ has been imported      **
    |** global._, definitions._ also imported    **
    |** Try  :help, :vals, power.<tab>           **
  """.stripMargin.trim

  def banner = """
    |Power mode enabled. :phase is at typer.
    |import scala.tools.nsc._, intp.global._, definitions._
    |Try :help or completions for vals._ and power._
  """.stripMargin.trim

  val initImports = List(
    "import scala.tools.nsc._",
    "import scala.jdk.CollectionConverters._",
    "import intp.global.{ error => _, _ }",
    "import definitions.{ getClass => _, _ }",
    "import power.rutil._",
    "import replImplicits._",
    "import treedsl.CODE._")

  /** Quietly starts up power mode and runs whatever is in init.
   */
  def unleash(): Unit = intp.reporter.withoutPrintingResults {
    // First we create the ReplVals instance and bind it to $r
    intp.bind("$r", replVals)
    // Then we import everything from $r.
    intp interpret s"import ${ intp.originalPath("$r") }._"
  }

  trait LowPriorityInternalInfo {
    implicit def apply[T: ru.TypeTag : ClassTag] : InternalInfo[T] = new InternalInfo[T](None)
  }
  object InternalInfo extends LowPriorityInternalInfo { }

  /** Now dealing with the problem of accidentally calling a method on Type
   *  when you're holding a Symbol and seeing the Symbol converted to the
   *  type of Symbol rather than the type of the thing represented by the
   *  symbol, by only implicitly installing one method, "?", and the rest
   *  of the conveniences exist on that wrapper.
   */
  trait LowPriorityInternalInfoWrapper { }
  class InternalInfoWrapper[T: ru.TypeTag : ClassTag](value: Option[T] = None) {
    def ? : InternalInfo[T] = new InternalInfo[T](value)
  }

  /** Todos...
   *    translate tag type arguments into applied types
   *    customizable symbol filter (had to hardcode no-spec to reduce noise)
   */
  class InternalInfo[T](value: Option[T] = None)(implicit typeEvidence: ru.TypeTag[T], runtimeClassEvidence: ClassTag[T]) {
    private def isSpecialized(s: Symbol) = s.name.toString contains "$mc"

    /** Standard noise reduction filter. */
    def excludeMember(s: Symbol) = (
         isSpecialized(s)
      || s.isAnonOrRefinementClass
      || s.isAnonymousFunction
    )
    def symbol            = definitions.compilerSymbolFromTag(tag)
    def tpe               = definitions.compilerTypeFromTag(tag)
    def members           = membersUnabridged filterNot excludeMember
    def membersUnabridged = tpe.members.toList
    def pkg               = symbol.enclosingPackage
    def tag               = typeEvidence
    def runtimeClass      = runtimeClassEvidence.runtimeClass
    def shortClass        = runtimeClass.getName.split("[$.]").last
    def baseClasses       = tpe.baseClasses

    override def toString = value match {
      case Some(x)  => "%s (%s)".format(x, shortClass)
      case _        => runtimeClass.getName
    }
  }

  trait LowPriorityPrettifier {
    implicit object AnyPrettifier extends Prettifier[Any] {
      def show(x: Any): Unit = prettify(x).iterator foreach println
      def prettify(x: Any): IterableOnce[String] = x match {
        case x: Name                => List(x.decode)
        case Tuple2(k, v)           => List(prettify(k).iterator ++ Iterator("->") ++ prettify(v) mkString " ")
        case xs: Array[_]           => xs.iterator flatMap prettify
        case xs: IterableOnce[_]    => xs.iterator flatMap prettify
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
    def default[T] = new Prettifier[T] {
      def prettify(x: T): IterableOnce[String] = AnyPrettifier prettify x
      def show(x: T): Unit = AnyPrettifier show x
    }
  }
  trait Prettifier[T] {
    def show(x: T): Unit
    def prettify(x: T): IterableOnce[String]

    def prettify(xs: IterableOnce[T]): IterableOnce[String] = xs.iterator flatMap (x => prettify(x))
  }

  abstract class PrettifierClass[T: Prettifier]() {
    val pretty = implicitly[Prettifier[T]]
    def value: Seq[T]

    def pp(f: Seq[T] => Seq[T]): Unit =
      pretty.prettify(f(value)).iterator foreach (StringPrettifier show _)

    def freq[U](p: T => U) = value.groupMapReduce(p)(_ => 1)(_ + _).toList sortBy (-_._2) map (_.swap)

    def >>(implicit ord: Ordering[T]): Unit      = pp(_.sorted)
    def >!(): Unit                               = pp(_.distinct)
    def >(): Unit                                = pp(identity)
  }

  class MultiPrettifierClass[T: Prettifier](val value: Seq[T]) extends PrettifierClass[T]() { }
  class SinglePrettifierClass[T: Prettifier](single: T) extends PrettifierClass[T]() {
    val value = List(single)
  }

  class RichReplString(s: String) {
    // make an url out of the string
    def u: URL = (
      if (s contains ":") new URI(s).toURL
      else if (new java.io.File(s).exists) new java.io.File(s).toURI.toURL
      else new URI("http://" + s).toURL
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

  trait Implicits1 {
    // fallback
    implicit def replPrinting[T](x: T)(implicit pretty: Prettifier[T] = Prettifier.default[T]): PrettifierClass[T] =
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
    implicit lazy val powerSymbolOrdering: Ordering[Symbol] = Ordering[Name] on (_.name)
    implicit lazy val powerTypeOrdering: Ordering[Type]     = Ordering[Symbol] on (_.typeSymbol)

    implicit def replInternalInfo[T: ru.TypeTag : ClassTag](x: T): InternalInfoWrapper[T] = new InternalInfoWrapper[T](Some(x))
    implicit def replEnhancedStrings(s: String): RichReplString = new RichReplString(s)
    implicit def replMultiPrinting[T: Prettifier](xs: IterableOnce[T]): MultiPrettifierClass[T] =
      new MultiPrettifierClass[T](Seq.from(xs))
    implicit def replPrettifier[T] : Prettifier[T] = Prettifier.default[T]
    implicit def replTypeApplication(sym: Symbol): RichSymbol = new RichSymbol(sym)

    implicit def replInputStream(in: InputStream)(implicit codec: Codec): RichInputStream = new RichInputStream(in)
    implicit def replEnhancedURLs(url: URL)(implicit codec: Codec): RichReplURL = new RichReplURL(url)(codec)
  }

  trait ReplUtilities {
    def module[T: ru.TypeTag] = ru.typeOf[T].typeSymbol.suchThat(_.isPackage)
    def clazz[T: ru.TypeTag] = ru.typeOf[T].typeSymbol.suchThat(_.isClass)
    def info[T: ru.TypeTag : ClassTag] = InternalInfo[T]
    def ?[T: ru.TypeTag : ClassTag] = InternalInfo[T]
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

  def unit(code: String)    = newCompilationUnit(code)
  def trees(code: String)   = intp.parse(code).map(_._1).getOrElse(Nil)

  override def toString = s"""
    |** Power mode status **
    |Default phase: ${phased.get}
    |Names: ${intp.unqualifiedIds mkString " "}
  """.stripMargin
}
