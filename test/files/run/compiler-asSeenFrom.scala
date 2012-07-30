import scala.tools.nsc._
import scala.tools.partest.CompilerTest
import scala.collection.{ mutable, immutable, generic }

/** It's too messy but it's better than not having it.
 */
object Test extends CompilerTest {
  import global._
  import definitions._

  override def sources = List(lambdaLift)
  def lambdaLift = """
package ll {
  class A1
  class A2
  class X
  class C[T1]() {
    class I[T2]() {
      def t1(): T1 = ???
      def t2(): T2 = ???
      def thisC(): C.this.type = ???
      def thisI(): I.this.type = ???
    }
  }
  class D[T3]() extends C[T3]() {
    val cD: C[List[T3]] = ???
    class J[T4]() extends cD.I[T4]()
  }
  object Z {
    val dZ:    D[A1] = ???
    val jZ: dZ.J[A2] = ???

    def kz[P <: dZ.J[A2]]: dZ.J[P] = ???
  }
}
"""

  object syms extends SymsInPackage("ll") {
    def isPossibleEnclosure(encl: Symbol, sym: Symbol) = sym.enclClassChain drop 1 exists (_ isSubClass encl)
    def isInterestingPrefix(pre: Type) = pre.typeConstructor.typeParams.nonEmpty && pre.members.exists(_.isType)

    def asSeenPrefixes  = tpes map (_.finalResultType) distinct
    def typeRefPrefixes = asSeenPrefixes filter isInterestingPrefix

    def nestsIn(outer: Symbol) = classes filter (c => c.enclClassChain drop 1 exists(_ isSubClass outer))
    def typeRefs(targs: List[Type]) = (
      for (p <- typeRefPrefixes ; c <- classes filter (isPossibleEnclosure(p.typeSymbol, _)) ; a <- targs) yield
        typeRef(p, c, List(a))
    )

    val wfmt      = "%-" + 25 + "s"
    def to_s(x: Any): String    = wfmt.format(x.toString.replaceAll("""\bll\.""", ""))

    def fmt(args: Any*): String = {
      (args map to_s mkString "  ").replaceAll("""\s+$""", "")
    }
    def fname(sym: Symbol) = {
      val p = "" + sym.owner.name
      val x = if (sym.owner.isPackageClass || sym.owner.isModuleClass || sym.owner.isTerm) "." else "#"
      sym.kindString + " " + p + x + sym.name
    }

    def permuteAsSeenFrom(targs: List[Type]) = (
      for {
        tp <- typeRefs(targs filterNot (_ eq NoType))
        prefix <- asSeenPrefixes
        if tp.prefix != prefix
        site <- classes
        seen = tp.asSeenFrom(prefix, site)
        if tp != seen
        if !seen.isInstanceOf[ExistentialType]
      }
      yield ((site, tp, prefix, seen))
    )

    def block(label: Any)(lines: List[String]): List[String] = {
      val first = "" + label + " {"
      val  last = "}"

      first +: lines.map("  " + _) :+ last
    }

    def permute(targs: List[Type]): List[String] = {
      permuteAsSeenFrom(targs).groupBy(_._1).toList.sortBy(_._1.toString) flatMap {
        case (site, xs) =>
          block(fmt(site)) {
            fmt("type", "seen from prefix", "is") ::
            fmt("----", "----------------", "--") :: {
              xs.groupBy(_._2).toList.sortBy(_._1.toString) flatMap {
                case (tp, ys) =>
                  (ys map { case (_, _, prefix, seen) => fmt(tp, prefix, seen) }).sorted.distinct
              }
            }
          }
      }
    }
  }

  def pretty(xs: List[_]) = if (xs.isEmpty) "" else xs.mkString("\n  ", "\n  ", "\n")

  def signaturesIn(info: Type): List[String] = (
    info.members.toList
      filterNot (s => s.isType || s.owner == ObjectClass || s.owner == AnyClass || s.isConstructor)
      map (_.defString)
  )

  def check(source: String, unit: global.CompilationUnit) = {
    import syms._

    afterTyper {
      val typeArgs = List[Type](IntClass.tpe, ListClass[Int]) ++ tparams.map(_.tpe)
      permute(typeArgs) foreach println
    }
    for (x <- classes ++ terms) {
      afterEachPhase(signaturesIn(x.tpe)) collect {
        case (ph, sigs) if sigs.nonEmpty =>
          println(sigs.mkString(x + " { // after " + ph + "\n  ", "\n  ", "\n}\n"))
      }
    }
    true
  }
}
