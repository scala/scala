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

  def missingParameterTypeError(fun: Tree, vparam: ValDef) = {
    val suffix = if (vparam.mods.isSynthetic) " for expanded function "+fun else ""

    inferError(vparam.pos, "missing parameter type" + suffix)
    ErrorType
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

  def applyErrorMsg(tree: Tree, msg: String, argtpes: List[Type], pt: Type) = {
    def asParams(xs: List[Any]) = xs.mkString("(", ", ", ")")

    def resType   = if (isWildcard(pt)) "" else " with expected result type " + pt
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

  /** If two given types contain different type variables with the same name
   *  differentiate the names by including owner information.  Also, if the
   *  type error is because of a conflict between two identically named
   *  classes and one is in package scala, fully qualify the name so one
   *  need not deduce why "java.util.Iterator" and "Iterator" don't match.
   *  Another disambiguation performed is to address the confusion present
   *  in the following snippet:
   *    def f[Int](x: Int) = x + 5.
   */
  def withDisambiguation[T](types: Type*)(op: => T): T = {
    object SymExtractor {
      def unapply(x: Any) = x match {
        case t @ TypeRef(_, sym, _)   => Some(t -> sym)
        case t @ ConstantType(value)  => Some(t -> t.underlying.typeSymbol)
        case _                        => None
      }
    }
    val typerefs =
      for (tp <- types.toList ; SymExtractor(t, sym) <- tp) yield
        t -> sym

    val savedNames    = typerefs map { case (_, sym) => sym -> sym.name } toMap
    def restoreNames  = savedNames foreach { case (sym, name) => sym.name = name }

    def isAlreadyAltered(sym: Symbol) = sym.name != savedNames(sym)

    def modifyName(sym: Symbol)(f: String => String): Unit =
      sym.name = newTypeName(f(sym.name.toString))

    def scalaQualify(sym: Symbol) =
      if (sym.owner.isScalaPackageClass)
        modifyName(sym)("scala." + _)

    def explainName(sym: Symbol) = {
      scalaQualify(sym)

      if (!isAlreadyAltered(sym))
        modifyName(sym)(_ + "(in " + sym.owner + ")")
    }

    ultimately(restoreNames) {
      for ((t1, sym1) <- typerefs ; (t2, sym2) <- typerefs ; if sym1 != sym2 && (sym1 isLess sym2)) {

        if (t1.toString == t2.toString) {   // type variable collisions
          List(sym1, sym2) foreach explainName
          if (sym1.owner == sym2.owner)
            sym2.name = newTypeName("(some other)"+sym2.name)
        }
        else if (sym1.name == sym2.name) {  // symbol name collisions
          List(sym1, sym2) foreach { x =>
            if (x.owner.isScalaPackageClass)
              modifyName(x)("scala." + _)
            else if (x.isTypeParameterOrSkolem)
              explainName(x)
          }
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
