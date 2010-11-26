/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package typechecker

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.ControlThrowable
import scala.util.control.Exception.ultimately
import symtab.Flags._
import PartialFunction._

/** An interface to enable higher configurability of diagnostic messages
 *  regarding type errors.  This is barely a beginning as error messages are
 *  distributed far and wide across the codebase.  The plan is to partition
 *  error messages into some broad groups and provide some mechanism for
 *  being more or less verbose on a selective basis.  Possible groups include
 *  such examples as
 *
 *    arity errors
 *    kind errors
 *    variance errors
 *    ambiguity errors
 *    volatility/stability errors
 *    implementation restrictions
 *
 *  And more, and there is plenty of overlap, so it'll be a process.
 *
 *  @author Paul Phillips
 *  @version 1.0
 */
trait TypeDiagnostics {
  self: Analyzer =>

  import global._
  import definitions._
  import global.typer.infer

  private def currentUnit = currentRun.currentUnit

  /** For ease of debugging.  The mode definitions are in Typers.scala.
   */
  private val modeNameMap = Map[Int, String](
    (1 << 0)  -> "EXPRmode",
    (1 << 1)  -> "PATTERNmode",
    (1 << 2)  -> "TYPEmode",
    (1 << 3)  -> "SCCmode",
    (1 << 4)  -> "FUNmode",
    (1 << 5)  -> "POLYmode",
    (1 << 6)  -> "QUALmode",
    (1 << 7)  -> "TAPPmode",
    (1 << 8)  -> "SUPERCONSTRmode",
    (1 << 9)  -> "SNDTRYmode",
    (1 << 10) -> "LHSmode",
    (1 << 11) -> "<DOES NOT EXIST mode>",
    (1 << 12) -> "STARmode",
    (1 << 13) -> "ALTmode",
    (1 << 14) -> "HKmode",
    (1 << 15) -> "BYVALmode",
    (1 << 16) -> "TYPEPATmode"
  )
  def modeString(mode: Int): String =
    (modeNameMap filterKeys (bit => (bit & mode) != 0)).values mkString " "

  /** It can be quite difficult to know which of the many functions called "error"
   *  is being called at any given point in the compiler.  To alleviate this I am
   *  renaming such functions inside this trait based on where it originated.
   */
  def inferError(pos: Position, msg: String) = infer.error(pos, msg)

  /** The common situation of making sure nothing is erroneous could be
   *  nicer if Symbols, Types, and Trees all implemented some common interface
   *  in which isErroneous and similar would be placed.
   */
  def noErroneousTypes(tps: Type*)    = tps forall (x => !x.isErroneous)
  def noErroneousSyms(syms: Symbol*)  = syms forall (x => !x.isErroneous)
  def noErroneousTrees(trees: Tree*)  = trees forall (x => !x.isErroneous)

  /** For errors which are artifacts of the implementation: such messages
   *  indicate that the restriction may be lifted in the future.
   */
  def restrictionWarning(pos: Position, unit: CompilationUnit, msg: String): Unit =
    unit.warning(pos, "Implementation restriction: " + msg)
  def restrictionError(pos: Position, unit: CompilationUnit, msg: String): Unit =
    unit.error(pos, "Implementation restriction: " + msg)

  /** A map of Positions to addendums - if an error involves a position in
   *  the map, the addendum should also be printed.
   */
  private var addendums = mutable.Map[Position, () => String]()

  def setAddendum(pos: Position, msg: () => String) =
    if (pos != NoPosition)
      addendums(pos) = msg

  def withAddendum(pos: Position) = (_: String) + addendums.getOrElse(pos, () => "")()

  def decodeWithNamespace(name: Name): String = {
    val prefix = if (name.isTypeName) "type " else "value "
    prefix + name.decode
  }

  /** Does the positioned line assigned to t1 precede that of t2?
   */
  def linePrecedes(t1: Tree, t2: Tree) = t1.pos.isDefined && t1.pos.isDefined && t1.pos.line < t2.pos.line

  def notAMember(sel: Tree, qual: Tree, name: Name) = {
    def decoded = decodeWithNamespace(name)

    def msg: String = name match {
      case nme.CONSTRUCTOR    => qual.tpe.widen+" does not have a constructor"
      case _                  =>
        def memberOf = if (qual.tpe.typeSymbol.isTypeParameterOrSkolem) "type parameter " else ""
        def possibleCause =
          if (linePrecedes(qual, sel))
            "\npossible cause: maybe a semicolon is missing before `"+decoded+"'?"
          else
            ""

        decoded+" is not a member of "+ memberOf + qual.tpe.widen + possibleCause
    }
    inferError(sel.pos, withAddendum(qual.pos)(msg))
  }

  /** Only prints the parameter names if they're not synthetic,
   *  since "x$1: Int" does not offer any more information than "Int".
   */
  private def methodTypeErrorString(tp: Type) = tp match {
    case mt @ MethodType(params, resultType)  =>
      def forString =
        if (params exists (_.isSynthetic)) params map (_.tpe)
        else params map (_.defString)

       forString.mkString("(", ",", ")") + resultType
    case x                                    => x.toString
  }

  def alternatives(tree: Tree): List[Type] = tree.tpe match {
    case OverloadedType(pre, alternatives)  => alternatives map pre.memberType
    case _                                  => Nil
  }
  def alternativesString(tree: Tree) =
    alternatives(tree) map (x => "  " + methodTypeErrorString(x)) mkString ("", " <and>\n", "\n")

  def missingParameterTypeMsg(fun: Tree, vparam: ValDef) = {
    val suffix =
      if (!vparam.mods.isSynthetic) ""
      else " for expanded function" + (fun match {
        case Function(_, Match(_, _)) => "\n(see SLS 8.5, \"Pattern Matching Anonymous Functions\")"
        case _                        => " " + fun
      })

    "missing parameter type" + suffix
  }

  def treeSymTypeMsg(tree: Tree): String = {
    val sym               = tree.symbol
    def hasParams         = tree.tpe.paramSectionCount > 0
    def preResultString   = if (hasParams) ": " else " of type "

    def nullMessage       = "expression of type " + tree.tpe
    def overloadedMessage = "overloaded method " + sym + " with alternatives:\n" + alternativesString(tree)
    def moduleMessage     = "" + sym
    def defaultMessage    = moduleMessage + preResultString + tree.tpe
    def applyMessage      = defaultMessage + tree.symbol.locationString

    if (sym == null) nullMessage
    else if (sym.isOverloaded) overloadedMessage
    else if (sym.isModule) moduleMessage
    else if (sym.name == nme.apply) applyMessage
    else defaultMessage
  }

  def notEnoughArgumentsMsg(fun: Tree, missing: List[Symbol]): String = {
    val suffix = {
      if (missing.isEmpty) ""
      else {
        val keep = missing take 3 map (_.name)
        ".\nUnspecified value parameter%s %s".format(
          if (missing.tail.isEmpty) "" else "s",
          if (missing drop 3 nonEmpty) (keep :+ "...").mkString(", ")
          else keep.mkString("", ", ", ".")
        )
      }
    }

    "not enough arguments for " + treeSymTypeMsg(fun) + suffix
  }

  def applyErrorMsg(tree: Tree, msg: String, argtpes: List[Type], pt: Type) = {
    def asParams(xs: List[Any]) = xs.mkString("(", ", ", ")")

    def resType   = if (pt isWildcard) "" else " with expected result type " + pt
    def allTypes  = (alternatives(tree) flatMap (_.paramTypes)) ++ argtpes :+ pt

    withDisambiguation(allTypes: _*) {
      treeSymTypeMsg(tree) + msg + asParams(argtpes) + resType
    }
  }

  def disambiguate(ss: List[String]) = ss match {
    case Nil      => Nil
    case s :: ss  => s :: (ss map { case `s` => "(some other)"+s ; case x => x })
  }

  // todo: use also for other error messages
  def existentialContext(tp: Type) = tp.existentialSkolems match {
    case Nil  => ""
    case xs   => " where " + (disambiguate(xs map (_.existentialToString)) mkString ", ")
  }

  def foundReqMsg(found: Type, req: Type): String =
    withDisambiguation(found, req) {
      ";\n found   : " + found.toLongString + existentialContext(found) +
       "\n required: " + req + existentialContext(req)
    }

  case class TypeDiag(tp: Type, sym: Symbol) extends Ordered[TypeDiag] {
    // save the name because it will be mutated until it has been
    // distinguished from the other types in the same error message
    private val savedName = sym.name
    def restoreName()     = sym.name = savedName
    def isAltered         = sym.name != savedName
    def modifyName(f: String => String) =
      sym.name = newTypeName(f(sym.name.toString))

    // functions to manipulate the name
    def preQualify()   = modifyName(trueOwner.fullName + "." + _)
    def postQualify()  = modifyName(_ + "(in " + trueOwner + ")")
    def scalaQualify() = if (isInScalaOrPredef) preQualify()
    def typeQualify()  = if (sym.isTypeParameterOrSkolem) postQualify()
    def nameQualify()  = if (trueOwner.isPackageClass) preQualify() else postQualify()

    def trueOwner  = tp.typeSymbol.ownerSkipPackageObject
    def aliasOwner = tp.typeSymbolDirect.ownerSkipPackageObject
    def owners     = List(trueOwner, aliasOwner)

    def isInScalaOrPredef = owners exists {
      case ScalaPackageClass | PredefModuleClass => true
      case _                                     => false
    }

    def sym_==(other: TypeDiag)     = tp.typeSymbol == other.tp.typeSymbol
    def owner_==(other: TypeDiag)   = trueOwner == other.trueOwner
    def string_==(other: TypeDiag)  = tp.toString == other.tp.toString
    def name_==(other: TypeDiag)    = sym.name == other.sym.name

    def compare(other: TypeDiag) =
      if (this == other) 0
      else if (sym isLess other.sym) -1
      else 1

    override def toString = {
      """
      |tp = %s
      |tp.typeSymbol = %s
      |tp.typeSymbol.owner = %s
      |tp.typeSymbolDirect = %s
      |tp.typeSymbolDirect.owner = %s
      |isInScalaOrPredef = %s
      """.stripMargin.format(
        tp, tp.typeSymbol, tp.typeSymbol.owner, tp.typeSymbolDirect, tp.typeSymbolDirect.owner, isInScalaOrPredef
      )
    }
  }
  private def typeDiags(types: Type*): List[TypeDiag] = {
    object SymExtractor {
      def unapply(x: Any) = x match {
        case t @ ConstantType(_)    => Some(t -> t.underlying.typeSymbol)
        case t @ TypeRef(_, sym, _) => Some(t -> sym)
        case _                      => None
      }
    }

    for (tp <- types.toList ; SymExtractor(t, sym) <- tp) yield
      TypeDiag(t, sym)
  }

  /** The distinct pairs from an ordered list. */
  private def pairs[T <: Ordered[T]](xs: Seq[T]): Seq[(T, T)] = {
    for (el1 <- xs ; el2 <- xs ; if el1 < el2) yield
      ((el1, el2))
  }

  /** Given any number of types, alters the name information in the symbols
   *  until they can be distinguished from one another: then executes the given
   *  code.  The names are restored and the result is returned.
   */
  def withDisambiguation[T](types: Type*)(op: => T): T = {
    val typeRefs = typeDiags(types: _*)
    val toCheck  = pairs(typeRefs) filterNot { case (td1, td2) => td1 sym_== td2 }

    ultimately(typeRefs foreach (_.restoreName())) {
      for ((td1, td2) <- toCheck) {
        val tds = List(td1, td2)

        // If the types print identically, qualify them:
        //   a) If the dealiased owner is a package, the full path
        //   b) Otherwise, append (in <owner>)
        if (td1 string_== td2)
          tds foreach (_.nameQualify())

        // If they have the same simple name, and either of them is in the
        // scala package or predef, qualify with scala so it is not confusing why
        // e.g. java.util.Iterator and Iterator are different types.
        if (td1 name_== td2)
          tds foreach (_.scalaQualify())

        // If they still print identically:
        //   a) If they are type parameters with different owners, append (in <owner>)
        //   b) Failing that, the best we can do is append "(some other)" to the latter.
        if (td1 string_== td2) {
          if (td1 owner_== td2)
            td2.modifyName("(some other)" + _)
          else
            tds foreach (_.typeQualify())
        }
      }
      // performing the actual operation
      op
    }
  }

  trait TyperDiagnostics {
    self: Typer =>

    private def contextError(pos: Position, msg: String) = context.error(pos, msg)
    private def contextError(pos: Position, err: Throwable) = context.error(pos, err)

    def symWasOverloaded(sym: Symbol) = sym.owner.isClass && sym.owner.info.member(sym.name).isOverloaded
    def cyclicAdjective(sym: Symbol)  = if (symWasOverloaded(sym)) "overloaded" else "recursive"

    /** Returns Some(msg) if the given tree is untyped apparently due
     *  to a cyclic reference, and None otherwise.
     */
    def cyclicReferenceMessage(sym: Symbol, tree: Tree) = condOpt(tree) {
      case ValDef(_, _, tpt, _) if tpt.tpe == null        => "recursive "+sym+" needs type"
      case DefDef(_, _, _, _, tpt, _) if tpt.tpe == null  => List(cyclicAdjective(sym), sym, "needs result type") mkString " "
    }

    /** Report a type error.
     *
     *  @param pos0   The position where to report the error
     *  @param ex     The exception that caused the error
     */
    def reportTypeError(pos: Position, ex: TypeError) {
      if (ex.pos == NoPosition) ex.pos = pos
      if (!context.reportGeneralErrors) throw ex
      if (settings.debug.value) ex.printStackTrace()

      ex match {
        case CyclicReference(sym, info: TypeCompleter) =>
          contextError(ex.pos, cyclicReferenceMessage(sym, info.tree) getOrElse ex.getMessage())

          if (sym == ObjectClass)
            throw new FatalError("cannot redefine root "+sym)
        case _ =>
          contextError(ex.pos, ex)
      }
    }
  }
}
