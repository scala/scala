//package weaver

import scala.annotation._
import scala.collection.mutable.ListBuffer
import scala.language.experimental.macros
import scala.reflect.macros.blackbox, blackbox.Context

case class SourceLocation(loc: String)

case class Expectations(assertion: String)

trait ExpectMacro {

  def apply(value: Boolean)(implicit loc: SourceLocation): Expectations =
    macro ExpectMacro.applyImpl
}

object ExpectMacro {

  /**
   * Constructs [[Expectations]] from a boolean value.
   *
   * A macro is needed to support clues. The value expression may contain calls
   * to [[ClueHelpers.clue]], which generate clues for values under test.
   *
   * This macro constructs a local collection of [[Clues]] and adds the
   * generated clues to it. Calls to [[ClueHelpers.clue]] are rewritten to calls
   * to [[Clues.addClue]].
   *
   * After the value is evaluated, the [[Clues]] collection is used to contruct
   * [[Expectations]].
   */
  def applyImpl(c: blackbox.Context)(value: c.Tree)(loc: c.Tree): c.Tree = {

    import c.universe._
    val sourcePos = c.enclosingPosition
    val sourceCode =
      new String(sourcePos.source.content.slice(sourcePos.start, sourcePos.end))

    val (cluesName, cluesValDef) = makeClues(c)
    val clueMethodSymbol         = getClueMethodSymbol(c)

    val transformedValue =
      replaceClueMethodCalls(c)(clueMethodSymbol, cluesName, value)
    makeExpectations(c)(cluesName = cluesName,
                        cluesValDef = cluesValDef,
                        value = transformedValue,
                        loc = loc,
                        sourceCode = sourceCode,
                        message = q"None")
  }

  /** Constructs [[Expectations]] from the local [[Clues]] collection. */
  private def makeExpectations(c: blackbox.Context)(
      cluesName: c.TermName,
      cluesValDef: c.Tree,
      value: c.Tree,
      loc: c.Tree,
      sourceCode: String,
      message: c.Tree): c.Tree = {
    import c.universe._
    //val sanitizedSourceCode = SourceCode.sanitize(c)(sourceCode)
    val block =
      q"$cluesValDef; Clues.toExpectations($loc, Some($sourceCode), $message, $cluesName, $value)"
      //q"$cluesValDef; _root_.weaver.internals.Clues.toExpectations($loc, Some($sanitizedSourceCode), $message, $cluesName, $value)"
    val untyped = c.untypecheck(block)
    val retyped = c.typecheck(untyped, pt = c.typeOf[Expectations])
    retyped
  }

  /** Get the [[ClueHelpers.clue]] symbol. */
  private def getClueMethodSymbol(c: blackbox.Context): c.Symbol = {
    import c.universe._
    symbolOf[ClueHelpers].info.member(TermName("clue"))
  }

  /** Construct a [[Clues]] collection local to the `expect` call. */
  private def makeClues(c: blackbox.Context): (c.TermName, c.Tree) = {
    import c.universe._
    val cluesName = TermName(c.freshName("clues$"))
    val cluesValDef =
      q"val $cluesName: Clues = new Clues()"
    (cluesName, cluesValDef)
  }

  /**
   * Replaces all calls to [[ClueHelpers.clue]] with calls to [[Clues.addClue]].
   */
  private def replaceClueMethodCalls(c: blackbox.Context)(
      clueMethodSymbol: c.Symbol,
      cluesName: c.TermName,
      value: c.Tree): c.Tree = {

    import c.universe._

    // This transformation outputs code that adds clues to a local
    // clues collection `cluesName`. It recurses over the input code and replaces
    // all calls of `ClueHelpers.clue` with `cluesName.addClue`.
    object transformer extends Transformer {

      override def transform(input: Tree): Tree = input match {
        case c.universe.Apply(fun, List(clueValue))
            if fun.symbol == clueMethodSymbol =>
          // The input tree corresponds to `ClueHelpers.clue(clueValue)` .
          // Transform it into `clueName.addClue(clueValue)`
          // Apply the transformation recursively to `clueValue` to support nested clues.
          val transformedClueValue = super.transform(clueValue)
          q"""${cluesName}.addClue($transformedClueValue)"""
        case o =>
          // Otherwise, recurse over the input.
          super.transform(o)
      }
    }

    transformer.transform(value)
  }
}

trait Show[T] {
  def show(t: T): String
}
object Show {
  implicit val showString: Show[String] = new Show[String] { def show(s: String) = s }
  implicit val showAny: Show[Any] = new Show[Any] { def show(x: Any) = x.toString }
}

trait ClueHelpers {

  // This function is removed as part of the `expect` macro expansion.
  @compileTimeOnly("This function can only be used within `expect`.")
  final def clue[A](@unused a: Clue[A]): A = ???
}

class Clue[T](
    source: String,
    val value: T,
    valueType: String,
    show: Show[T]
) {
  def prettyPrint: String =
    s"${source}: ${valueType} = ${show.show(value)}"
}
object Clue extends LowPriorityClueImplicits {

  /**
   * Generates a clue for a given value using a [[Show]] instance to print the
   * value.
   */
  implicit def generateClue[A](value: A)(implicit catsShow: Show[A]): Clue[A] =
    macro ClueMacro.impl
}
trait LowPriorityClueImplicits {

  /**
   * Generates a clue for a given value using the [[toString]] function to print
   * the value.
   */
  implicit def generateClueFromToString[A](value: A): Clue[A] =
    macro ClueMacro.showFromToStringImpl
}
object ClueMacro {
  def showFromToStringImpl(c: Context)(value: c.Tree): c.Tree = {
    import c.universe._
    impl(c)(value)(q"Show.showAny")
  }

  /**
   * Constructs a clue by extracting the source code and type information of a
   * value.
   */
  def impl(c: Context)(value: c.Tree)(catsShow: c.Tree): c.Tree = {
    import c.universe._
    val text: String =
      if (value.pos != null && value.pos.isRange) {
        val chars = value.pos.source.content
        val start = value.pos.start
        val end   = value.pos.end
        if (end > start &&
          start >= 0 && start < chars.length &&
          end >= 0 && end < chars.length) {
          new String(chars, start, end - start)
        } else {
          ""
        }
      } else {
        ""
      }
    def simplifyType(tpe: Type): Type = tpe match {
      case TypeRef(ThisType(pre), sym, args) if pre == sym.owner =>
        simplifyType(c.internal.typeRef(NoPrefix, sym, args))
      case t =>
        t.widen
    }
    val source    = Literal(Constant(text.trim))
    val valueType = Literal(Constant(simplifyType(value.tpe).toString()))
    val clueTpe = c.internal.typeRef(
      NoPrefix,
      c.mirror.staticClass(classOf[Clue[_]].getName()),
      List(value.tpe.widen)
    )
    q"new $clueTpe(..$source, $value, $valueType, $catsShow)"
  }
}

final class Clues {
  private val clues: ListBuffer[Clue[?]] = ListBuffer.empty

  /**
   * Adds a clue to the collection.
   *
   * This function is called as part of the expansion of the `expect` macro. It
   * should not be called explicitly.
   */
  def addClue[A](clue: Clue[A]): A = {
    clues.addOne(clue)
    clue.value
  }

  def getClues: List[Clue[?]] = clues.toList
}

object Clues {

  /**
   * Constructs [[Expectations]] from the collection of clues.
   *
   * If the result is successful, the clues are discarded. If the result has
   * failed, the clues are printed as part of the failure message.
   *
   * This function is called as part of the expansion of the `expect` macro. It
   * should not be called explicitly.
   */
  def toExpectations(
      sourceLoc: SourceLocation,
      sourceCode: Option[String],
      message: Option[String],
      clues: Clues,
      success: Boolean): Expectations = {
    if (success) {
      Expectations("success")
    } else {
      val header = "assertion failed" + message.fold("")(msg => s": $msg")
      val sourceCodeMessage = sourceCode.fold("")(msg => s"\n\n$msg")
      val clueList          = clues.getClues
      val cluesMessage = if (clueList.nonEmpty) {
        val lines = clueList.map(clue => s"  ${clue.prettyPrint}")
        lines.mkString("Clues {\n", "\n", "\n}")
      } else "Use the `clue` function to troubleshoot"
      val fullMessage = header + sourceCodeMessage + "\n\n" + cluesMessage
      Expectations(fullMessage)
    }
  }
}
