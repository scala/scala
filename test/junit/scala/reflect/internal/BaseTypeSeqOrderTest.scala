package scala.reflect.internal

import org.junit.Assert.fail
import org.junit.Test

import scala.tools.testkit.BytecodeTesting

/** `lubList` needs base type sequences sorted by `Symbol#isLess`. This checks that an abstract type
 *  bounded by a class still sorts before that class, over a grid of shapes where the class is only
 *  known seen from a prefix.
 */
object BaseTypeSeqOrderTest {
  /** @param apiBound     bound of the abstract `Name` as declared in `Api`
   *  @param implParents  extra parents of the class `Name` in `Impl`; varies its rank relative to `apiBound`
   *  @param memberBound  bound of `Sym#NameType`, in terms of `Name`
   *  @param apiFirst     whether `Api` is declared first, which decides symbol ids in a tie
   */
  final case class Shape(apiBound: String, implParents: Int, memberBound: String, apiFirst: Boolean) {
    private def api =
      s"""trait Api {
         |  trait NameApi; trait E; trait Marker
         |  type Name >: Null <: $apiBound
         |  trait Sym {
         |    type U >: Null <: Name
         |    type NameType >: Null <: $memberBound
         |    def name: NameType
         |  }
         |}
         |""".stripMargin

    private def impl = {
      val traits = List.tabulate(implParents)(i => s"T$i")
      val parents = ("NameApi" :: "E" :: traits).mkString(" with ")
      s"""trait Impl extends Api {
         |  ${traits.map("trait " + _).mkString("; ")}
         |  abstract class Name extends $parents
         |  class TermName extends Name
         |  val s: Sym
         |  val s2: Sym
         |  def classBound: Name
         |  def termName: TermName
         |  def memberName: s.NameType
         |  def otherMemberName: s2.NameType
         |  def inferred(b: Boolean) = if (b) termName else s.name
         |}
         |""".stripMargin
    }

    def source: String = if (apiFirst) api + impl else impl + api
    override def toString = s"apiBound=[$apiBound] implParents=$implParents memberBound=[$memberBound] apiFirst=$apiFirst"
  }

  val shapes: List[Shape] =
    for {
      apiBound    <- List("AnyRef", "AnyRef with NameApi", "NameApi", "NameApi with E")
      implParents <- List(0, 1, 2, 3, 4)
      memberBound <- List("Name", "Name with Marker", "U", "Name { def x: Int }")
      apiFirst    <- List(true, false)
    } yield Shape(apiBound, implParents, memberBound, apiFirst)
}

class BaseTypeSeqOrderTest extends BytecodeTesting {
  import BaseTypeSeqOrderTest._

  override def compilerArgs = "-Ystop-after:typer"

  import compiler.global
  import global._

  /** A refinement in an abstract type's bound may sort ahead of it. That can't lose it from a lub,
   *  as no other operand has that refinement class.
   */
  private def isRefinementAfterAbstract(a: Type, b: Type) = a.typeSymbol.isAbstractType && b.typeSymbol.isRefinementClass

  private def unsorted(tp: Type): List[String] = {
    val elems = tp.baseTypeSeq.toList
    elems.zip(elems.tail).collect {
      case (a, b) if !(a.typeSymbol isLess b.typeSymbol) && !isRefinementAfterAbstract(a, b) =>
        s"base types of $tp are out of order: $a before $b"
    }
  }

  private def notStrictTotalOrder(syms: List[Symbol]): List[String] = {
    val irreflexive = syms.collect { case a if a isLess a => s"$a isLess itself" }
    val total = for {
      a <- syms; b <- syms if a ne b
      if (a isLess b) == (b isLess a)
    } yield s"exactly one of $a isLess $b and the converse must hold"
    val transitive = for {
      a <- syms; b <- syms; c <- syms
      if (a isLess b) && (b isLess c) && !(a isLess c)
    } yield s"$a isLess $b isLess $c, but not $a isLess $c"
    irreflexive ::: total ::: transitive
  }

  /** The problems with this shape, and the type it infers. */
  private def check(shape: Shape): (List[String], String) = {
    val run = compiler.newRun()
    run.compileSources(List(BytecodeTesting.makeSourceFile(shape.source, "unitTestSource.scala")))
    compiler.checkReport()
    exitingTyper {
      val impl = rootMirror.EmptyPackageClass.info.member(TypeName("Impl"))
      def resultOf(name: String) = impl.thisType.memberInfo(impl.info.member(TermName(name))).finalResultType
      val classBound      = resultOf("classBound")
      val termName        = resultOf("termName")
      val memberName      = resultOf("memberName")
      val otherMemberName = resultOf("otherMemberName")
      val inferred        = resultOf("inferred")

      val operands = List(termName, memberName)
      val syms = (classBound :: operands).flatMap(_.baseTypeSeq.toList.map(_.typeSymbol)).distinct

      def keepsBound(what: String, tp: Type) =
        if (tp <:< classBound) Nil else List(s"$what is $tp, which does not conform to $classBound")

      val problems =
        operands.flatMap(unsorted) :::
        notStrictTotalOrder(syms) :::
        keepsBound("the lub", lub(operands)) :::
        keepsBound("the lub with operands swapped", lub(operands.reverse)) :::
        keepsBound("the inferred type", inferred) :::
        keepsBound("the lub of two prefixes", lub(List(memberName, otherMemberName))) :::
        keepsBound("the lub of three", lub(List(termName, memberName, otherMemberName))) :::
        keepsBound("the lub with the bound", lub(List(memberName, classBound)))
      (problems, inferred.toString)
    }
  }

  @Test def abstractTypeSortsBeforeClassInItsBound(): Unit = {
    val results = shapes.map(shape => shape -> check(shape))
    val problems = results.flatMap { case (shape, (ps, _)) => ps.map(s"$shape: " + _) }
    val orderDependent = results
      .groupMap(_._1.copy(apiFirst = true))(_._2._2)
      .collect { case (shape, inferred) if inferred.distinct.lengthIs > 1 =>
        s"$shape: inferred type depends on declaration order: ${inferred.distinct.mkString(" vs ")}"
      }
    val all = problems ++ orderDependent
    if (all.nonEmpty)
      fail(s"${all.size} problems, the first of which are:\n${all.take(10).mkString("\n")}")
  }
}
