/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
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
  import global.typer.{ infer, context }

  private def currentUnit = currentRun.currentUnit

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
  private var addendums = perRunCaches.newMap[Position, () => String]()

  def setAddendum(pos: Position, msg: () => String) =
    if (pos != NoPosition)
      addendums(pos) = msg

  def withAddendum(pos: Position) = (_: String) + addendums.getOrElse(pos, () => "")()

  def decodeWithKind(name: Name, owner: Symbol): String = {
    val prefix = (
      if (name.isTypeName) "type "
      else if (owner.isPackageClass) "object "
      else "value "
    )
    prefix + name.decode
  }

  /** Does the positioned line assigned to t1 precede that of t2?
   */
  def posPrecedes(p1: Position, p2: Position) = p1.isDefined && p2.isDefined && p1.line < p2.line
  def linePrecedes(t1: Tree, t2: Tree) = posPrecedes(t1.pos, t2.pos)

  private object DealiasedType extends TypeMap {
    def apply(tp: Type): Type = tp match {
      // Avoid "explaining" that String is really java.lang.String,
      // while still dealiasing types from non-default namespaces.
      case TypeRef(pre, sym, args) if sym.isAliasType && !sym.isInDefaultNamespace =>
        mapOver(tp.dealias)
      case _ =>
        mapOver(tp)
    }
  }

  def notAMemberMessage(pos: Position, qual: Tree, name: Name) = {
    val owner            = qual.tpe.typeSymbol
    val target           = qual.tpe.widen
    def targetKindString = if (owner.isTypeParameterOrSkolem) "type parameter " else ""
    def nameString       = decodeWithKind(name, owner)
    /** Illuminating some common situations and errors a bit further. */
    def addendum         = {
      val companion = {
        if (name.isTermName && owner.isPackageClass) {
          target.member(name.toTypeName) match {
            case NoSymbol => ""
            case sym      => "\nNote: %s exists, but it has no companion object.".format(sym)
          }
        }
        else ""
      }
      val semicolon = (
        if (posPrecedes(qual.pos, pos))
          "\npossible cause: maybe a semicolon is missing before `"+nameString+"'?"
        else
          ""
      )
      companion + semicolon
    }

    withAddendum(qual.pos)(
      if (name == nme.CONSTRUCTOR) target + " does not have a constructor"
      else nameString + " is not a member of " + targetKindString + target + addendum
    )
  }

  /** An explanatory note to be added to error messages
   *  when there's a problem with abstract var defs */
  def abstractVarMessage(sym: Symbol): String =
    if (underlyingSymbol(sym).isVariable)
      "\n(Note that variables need to be initialized to be defined)"
    else ""

  def notAMemberError(pos: Position, qual: Tree, name: Name) =
    context.error(pos, notAMemberMessage(pos, qual, name))

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

  def missingParameterTypeMsg(fun: Tree, vparam: ValDef, pt: Type) = {
    def anonMessage = (
      "\nThe argument types of an anonymous function must be fully known. (SLS 8.5)" +
      "\nExpected type was: " + pt.toLongString
    )
    val suffix =
      if (!vparam.mods.isSynthetic) ""
      else " for expanded function" + (fun match {
        case Function(_, Match(_, _)) => anonMessage
        case _                        => " " + fun
      })

    "missing parameter type" + suffix
  }

  /** The symbol which the given accessor represents (possibly in part).
   *  This is used for error messages, where we want to speak in terms
   *  of the actual declaration or definition, not in terms of the generated setters
   *  and getters.
   */
  def underlyingSymbol(member: Symbol): Symbol =
    if (!member.hasAccessorFlag) member
    else if (!member.isDeferred) member.accessed
    else {
      val getter = if (member.isSetter) member.getter(member.owner) else member
      val flags  = if (getter.setter(member.owner) != NoSymbol) DEFERRED | MUTABLE else DEFERRED

      ( getter.owner.newValue(getter.pos, getter.name.toTermName)
          setInfo getter.tpe.resultType
          setFlag flags
      )
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
    def locals    = alternatives(tree) flatMap (_.typeParams)

    withDisambiguation(locals, allTypes: _*) {
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

  def varianceWord(sym: Symbol): String =
    if (sym.variance == 1) "covariant"
    else if (sym.variance == -1) "contravariant"
    else "invariant"

  def explainAlias(tp: Type) = {
    // Don't automatically normalize standard aliases; they still will be
    // expanded if necessary to disambiguate simple identifiers.
    if ((tp eq tp.normalize) || tp.typeSymbolDirect.isInDefaultNamespace) ""
    else {
      // A sanity check against expansion being identical to original.
      val s = "" + DealiasedType(tp)
      if (s == "" + tp) ""
      else "\n    (which expands to)  " + s
    }
  }

  /** Look through the base types of the found type for any which
   *  might have been valid subtypes if given conformant type arguments.
   *  Examine those for situations where the type error would have been
   *  eliminated if the variance were different.  In such cases, append
   *  an additional explanatory message.
   *
   *  TODO: handle type aliases better.
   */
  def explainVariance(found: Type, req: Type): String = {
    found.baseTypeSeq.toList foreach { tp =>
      if (tp.typeSymbol isSubClass req.typeSymbol) {
        val foundArgs = tp.typeArgs
        val reqArgs   = req.typeArgs
        val params    = req.typeConstructor.typeParams

        if (foundArgs.nonEmpty && foundArgs.length == reqArgs.length) {
          val relationships = (foundArgs, reqArgs, params).zipped map {
            case (arg, reqArg, param) =>
              def mkMsg(isSubtype: Boolean) = {
                val op      = if (isSubtype) "<:" else ">:"
                val suggest = if (isSubtype) "+" else "-"
                val reqsym  = req.typeSymbol
                def isJava  = reqsym.isJavaDefined
                def isScala = reqsym hasTransOwner ScalaPackageClass

                val explainFound = "%s %s %s%s, but ".format(
                  arg, op, reqArg,
                  // If the message involves a type from the base type sequence rather than the
                  // actual found type, we need to explain why we're talking about it.  Less brute
                  // force measures than comparing normalized Strings were producing error messages
                  // like "and java.util.ArrayList[String] <: java.util.ArrayList[String]" but there
                  // should be a cleaner way to do this.
                  if (found.normalize.toString == tp.normalize.toString) ""
                  else " (and %s <: %s)".format(found, tp)
                )
                val explainDef = {
                  val prepend = if (isJava) "Java-defined " else ""
                  "%s%s is %s in %s.".format(prepend, reqsym, varianceWord(param), param)
                }
                // Don't suggest they change the class declaration if it's somewhere
                // under scala.* or defined in a java class, because attempting either
                // would be fruitless.
                val suggestChange = "\nYou may wish to " + (
                  if (isScala || isJava)
                    "investigate a wildcard type such as `_ %s %s`. (SLS 3.2.10)".format(op, reqArg)
                  else
                    "define %s as %s%s instead. (SLS 4.5)".format(param.name, suggest, param.name)
                )

                Some("Note: " + explainFound + explainDef + suggestChange)
              }
              // In these cases the arg is OK and needs no explanation.
              val conforms = (
                   (arg =:= reqArg)
                || ((arg <:< reqArg) && param.isCovariant)
                || ((reqArg <:< arg) && param.isContravariant)
              )
              val invariant = param.variance == 0

              if (conforms)                             Some("")
              else if ((arg <:< reqArg) && invariant)   mkMsg(true)   // covariant relationship
              else if ((reqArg <:< arg) && invariant)   mkMsg(false)  // contravariant relationship
              else None // we assume in other cases our ham-fisted advice will merely serve to confuse
          }
          val messages = relationships.flatten
          // the condition verifies no type argument came back None
          if (messages.size == foundArgs.size)
            return messages filterNot (_ == "") mkString ("\n", "\n", "")
        }
      }
    }
    ""    // no elaborable variance situation found
  }
  def foundReqMsg(found: Type, req: Type): String = (
    withDisambiguation(Nil, found, req)(
      ";\n found   : " + found.toLongString + existentialContext(found) + explainAlias(found) +
       "\n required: " + req + existentialContext(req) + explainAlias(req)
    ) + explainVariance(found, req)
  )

  case class TypeDiag(tp: Type, sym: Symbol) extends Ordered[TypeDiag] {
    // save the name because it will be mutated until it has been
    // distinguished from the other types in the same error message
    private val savedName = sym.name
    def restoreName()     = sym.name = savedName
    def isAltered         = sym.name != savedName
    def modifyName(f: String => String) =
      sym.name = newTypeName(f(sym.name.toString))

    /** Prepend java.lang, scala., or Predef. if this type originated
     *  in one of those.
     */
    def qualifyDefaultNamespaces() = {
      val intersect = Set(trueOwner, aliasOwner) intersect UnqualifiedOwners
      if (intersect.nonEmpty) preQualify()
    }

    // functions to manipulate the name
    def preQualify()   = modifyName(trueOwner.fullName + "." + _)
    def postQualify()  = modifyName(_ + "(in " + trueOwner + ")")
    def typeQualify()  = if (sym.isTypeParameterOrSkolem) postQualify()
    def nameQualify()  = if (trueOwner.isPackageClass) preQualify() else postQualify()

    def trueOwner  = tp.typeSymbol.effectiveOwner
    def aliasOwner = tp.typeSymbolDirect.effectiveOwner

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
      """.stripMargin.format(
        tp, tp.typeSymbol, tp.typeSymbol.owner, tp.typeSymbolDirect, tp.typeSymbolDirect.owner
      )
    }
  }
  private def typeDiags(locals: List[Symbol], types: Type*): List[TypeDiag] = {
    object SymExtractor {
      def unapply(x: Any) = x match {
        case t @ ConstantType(_)    => Some(t -> t.underlying.typeSymbol)
        case t @ TypeRef(_, sym, _) => if (locals contains sym) None else Some(t -> sym)
        case _                      => None
      }
    }

    for (tp <- types.toList; SymExtractor(t, sym) <- tp) yield TypeDiag(t, sym)
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
  def withDisambiguation[T](locals: List[Symbol], types: Type*)(op: => T): T = {
    val typeRefs = typeDiags(locals, types: _*)
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
          tds foreach (_.qualifyDefaultNamespaces())

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
    private def contextWarning(pos: Position, msg: String) = context.unit.warning(pos, msg)

    def permanentlyHiddenWarning(pos: Position, hidden: Name, defn: Symbol) =
      contextWarning(pos, "imported `%s' is permanently hidden by definition of %s".format(hidden, defn.fullLocationString))

    object checkDead {
      private var expr: Symbol = NoSymbol
      private def exprOK = expr != Object_synchronized
      private def treeOK(tree: Tree) = tree.tpe != null && tree.tpe.typeSymbol == NothingClass

      def updateExpr(fn: Tree) = {
        if (fn.symbol != null && fn.symbol.isMethod && !fn.symbol.isConstructor)
          checkDead.expr = fn.symbol
      }
      def apply(tree: Tree): Tree = {
        // Error suppression will squash some of these warnings unless we circumvent it.
        // It is presumed if you are using a -Y option you would really like to hear
        // the warnings you've requested.
        if (settings.warnDeadCode.value && context.unit.exists && treeOK(tree) && exprOK) {
          val saved = context.reportGeneralErrors
          try {
            context.reportGeneralErrors = true
            context.warning(tree.pos, "dead code following this construct")
          }
          finally context.reportGeneralErrors = saved
        }
        tree
      }

      // The checkDead call from typedArg is more selective.
      def inMode(mode: Int, tree: Tree): Tree = {
        val modeOK = (mode & (EXPRmode | BYVALmode | POLYmode)) == (EXPRmode | BYVALmode)
        if (modeOK) apply(tree)
        else tree
      }
    }

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
