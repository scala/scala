/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package util

import scala.collection.{ mutable, immutable, generic }

/** A class for analyzing forwarding/proxy relationships.
 */
trait ProxyReport {
  val global: Global
  import global._
  import definitions.{ getClass => gc, _ }

  private object classes {
    def isIgnorable(sym: Symbol) = sym :: sym.allOverriddenSymbols exists { s =>
      ObjectClass isSubClass s.owner
    }
    def nonPrivateMethods(sym: Symbol) = {
      val methods = sym.initialize.tpe.nonPrivateMembers filter { x =>
        x.isMethod && !x.isConstructor && !x.isPrivate && !isIgnorable(x)
      }
      methods foreach (m => m.initialize.info.paramss.flatten foreach (_.initialize))
      methods
    }
    lazy val GlobalClass     = gc(classOf[Global].getName)
    lazy val GenericClass    = getModule("scala.collection.generic").moduleClass
    lazy val CollectionClass = getModule("scala.collection").moduleClass

    def getType(name: String)    = getMember(GlobalClass, name.toTypeName)
    def getColl(name: String)    = getMember(CollectionClass, name.toTypeName)
    def getGeneric(name: String) = getMember(GenericClass, name.toTypeName)

    // the following operations + those in RewrappingTypeProxy are all operations
    // in class Type that are overridden in some subclass
    // Important to keep this up-to-date when new operations are added!
    def TypeClass             = getType("Type")
    def SimpleTypeProxy       = getType("SimpleTypeProxy")
    def RewrappingTypeProxy   = getType("RewrappingTypeProxy")

    def TraversableForwarder = getGeneric("TraversableForwarder")
    def IterableForwarder    = getGeneric("IterableForwarder")
    def SeqForwarder         = getGeneric("SeqForwarder")
    def TraversableLike      = getColl("TraversableLike")
    def TraversableProxy     = getColl("TraversableProxyLike")
    def IterableLike         = getColl("IterableLike")
    def IterableProxy        = getColl("IterableProxyLike")
    def MapLike              = getColl("MapLike")
    def MapProxy             = getColl("MapProxyLike")
    def SeqLike              = getColl("SeqLike")
    def SeqProxy             = getColl("SeqProxyLike")
    def SetLike              = getColl("SetLike")
    def SetProxy             = getColl("SetProxyLike")
  }
  import classes._

  val wrappedHeader = """
/** With respect to %s, %s wraps:
 */
trait Wrapped {
  """.trim
  val unwrappedHeader = """
/** With respect to %s, %s does NOT wrap:
 */
trait Unwrapped {
    """.trim

  def wrapReport(underlying: Symbol, proxy: Symbol) = {
    val underlyingMs         = nonPrivateMethods(underlying)
    val proxyMs              = nonPrivateMethods(proxy) filterNot (_.owner == underlying)
    val (wrapped, unwrapped) = underlyingMs partition (m =>
      proxyMs exists (p =>
        (p.name == m.name) && {
          val self = proxy.thisType
          val memberTp = self.memberType(p)
          val parentTp = self.memberType(m)

          refChecks.overridesTypeInPrefix(memberTp, parentTp, self)
          //  || {
          //   // if (p.paramss.flatten.length == m.paramss.flatten.length)
          //   //   println("names equal, overridesType false:\n  " + ((p, m, memberTp, parentTp, self)) + "\n")
          //
          //   false
          // }
        }
      )
    )

    def show(xs: List[Symbol], template: String) = {
      val lines = xs.map(_.initialize.defString).sorted.map("  " + _ + "\n")
      lines.mkString(template.format(underlying, proxy) + "\n", "", "}")
    }

    show(wrapped, wrappedHeader) + "\n\n" + show(unwrapped, unwrappedHeader)
  }

  lazy val wrappers = List(
    TypeClass        -> SimpleTypeProxy,
    TypeClass        -> RewrappingTypeProxy,
    TraversableClass -> TraversableForwarder,
    IterableClass    -> IterableForwarder,
    SeqClass         -> SeqForwarder,
    TraversableLike  -> TraversableProxy,
    IterableLike     -> IterableProxy,
    MapLike          -> MapProxy,
    SetLike          -> SetProxy,
    SeqLike          -> SeqProxy
  )

  def generate(dir: io.Directory) = {
    /** A proxy for a type (identified by field `underlying`) that forwards most
     *  operations to it (for exceptions, see WrappingProxy, which forwards even more operations).
     *  every operation that is overridden for some kind of types should be forwarded.
     */
    for ((clazz, proxy) <- wrappers) {
      val text = wrapReport(clazz, proxy)
      val file = dir / (proxy.fullName + ".scala") toFile;

      file writeAll text
      println("Created " + file)
    }
  }
}

object ProxyReportRunner {
  class ProxyGlobal(s: Settings) extends Global(s) {
    object proxyReport extends {
      val global: ProxyGlobal.this.type = ProxyGlobal.this
    } with util.ProxyReport
  }

  def main(args: Array[String]): Unit = {
    if (args.isEmpty)
      return println("Give an output directory as argument.")

    val dir = io.Directory(args(0)).createDirectory()
    val s = new Settings()
    s.processArguments(args.toList.tail, true)
    val g = new ProxyGlobal(s)
    val run = new g.Run()
    g.atPhase(run.typerPhase.next)(g.proxyReport.generate(dir))
  }
}
