/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package typechecker

import scala.reflect.internal.util.StringOps.{countAsString, countElementsAsString}
import java.lang.System.{lineSeparator => EOL}
import scala.PartialFunction.cond
import scala.annotation.tailrec
import scala.reflect.runtime.ReflectionUtils
import scala.reflect.macros.runtime.AbortMacroException
import scala.util.control.{ControlThrowable, NonFatal}
import scala.tools.nsc.Reporting.WarningCategory
import scala.tools.nsc.util.stackTraceString
import scala.reflect.io.NoAbstractFile
import scala.reflect.internal.util.{CodeAction, NoSourceFile}

trait ContextErrors extends splain.SplainErrors {
  self: Analyzer =>

  import global._
  import definitions._

  final case class ContextWarning(pos: Position, msg: String, cat: WarningCategory, sym: Symbol, actions: List[CodeAction])

  sealed abstract class AbsTypeError {
    def errPos: Position
    def errMsg: String
    override def toString() = "[Type error at:" + errPos + "] " + errMsg

    // To include code actions in type errors, add a field to the corresponding case class
    //   override val actions: List[CodeAction] = Nil
    // See TypeErrorWrapper for example
    def actions: List[CodeAction] = Nil
  }

  abstract class AbsAmbiguousTypeError extends AbsTypeError

  case class AmbiguousTypeError(errPos: Position, errMsg: String)
    extends AbsAmbiguousTypeError

  case class AmbiguousImplicitTypeError(underlyingTree: Tree, errMsg: String)
    extends AbsAmbiguousTypeError {
    def errPos = underlyingTree.pos
  }

  sealed abstract class TreeTypeError extends AbsTypeError {
    def underlyingTree: Tree
    def errPos = underlyingTree.pos
  }

  case class NormalTypeError(underlyingTree: Tree, errMsg: String, override val actions: List[CodeAction] = Nil)
    extends TreeTypeError

  case class AccessTypeError(underlyingTree: Tree, errMsg: String)
    extends TreeTypeError

  case class SymbolTypeError(underlyingSym: Symbol, errMsg: String)
    extends AbsTypeError {

    def errPos = underlyingSym.pos
  }

  case class TypeErrorWrapper(ex: TypeError, override val actions: List[CodeAction] = Nil)
    extends AbsTypeError {
    def errMsg = ex.msg
    def errPos = ex.pos
  }

  case class TypeErrorWithUnderlyingTree(tree: Tree, ex: TypeError)
    extends AbsTypeError {
    def errMsg = ex.msg
    def errPos = tree.pos
  }

  // Unlike other type errors diverging implicit expansion
  // will be re-issued explicitly on failed implicit argument search.
  // This is because we want to:
  // 1) provide better error message than just "implicit not found"
  // 2) provide the type of the implicit parameter for which we got diverging expansion
  //    (pt at the point of divergence gives less information to the user)
  // Note: it is safe to delay error message generation in this case
  // because we don't modify implicits' infos.
  case class DivergentImplicitTypeError(underlyingTree: Tree, pt0: Type, sym: Symbol)
    extends TreeTypeError {
    def errMsg: String   = errMsgForPt(pt0)
    def withPt(pt: Type): AbsTypeError = this.copy(pt0 = pt)
    private def errMsgForPt(pt: Type) =
      s"diverging implicit expansion for type ${pt}\nstarting with ${sym.fullLocationString}"
  }


  case class PosAndMsgTypeError(errPos: Position, errMsg: String)
    extends AbsTypeError

  object ErrorUtils {
    def issueNormalTypeError(tree: Tree, msg: String, actions: List[CodeAction] = Nil)(implicit context: Context): Unit = {
      issueTypeError(NormalTypeError(tree, msg, actions))
    }

    def issueSymbolTypeError(sym: Symbol, msg: String)(implicit context: Context): Unit = {
      issueTypeError(SymbolTypeError(sym, msg))
    }

    def issueTypeError(err: AbsTypeError)(implicit context: Context): Unit = { context.issue(err) }

    // OPT: avoid error string creation for errors that won't see the light of day
    def typeErrorMsg(context: Context, found: Type, req: Type) =
      if (!context.openImplicits.isEmpty && !settings.Vimplicits.value) "type mismatch"
      else "type mismatch" + foundReqMsg(found, req)
  }

  def notAnyRefMessage(found: Type): String = {
    val tp        = found.widen
    def name      = tp.typeSymbol.nameString
    def parents   = tp.parents filterNot isTrivialTopType
    def onlyAny   = tp.parents forall (_.typeSymbol == AnyClass)
    def parents_s = ( if (parents.isEmpty) tp.parents else parents ) mkString ", "
    def what = (
      if (tp.typeSymbol.isAbstractType) {
        val descr = if (onlyAny) "unbounded" else "bounded only by " + parents_s
        s"$name is $descr, which means AnyRef is not a known parent"
      }
      else if (tp.typeSymbol.isAnonOrRefinementClass)
        s"the parents of this type ($parents_s) extend Any, not AnyRef"
      else
        s"$name extends Any, not AnyRef"
    )
    if (isPrimitiveValueType(found) || isTrivialTopType(tp)) "" else
       sm"""|Note that $what.
            |Such types can participate in value classes, but instances
            |cannot appear in singleton types or in reference comparisons."""
  }

  import ErrorUtils._

  private def MacroIncompatibleEngineError(friendlyMessage: String, internalMessage: String) = {
    def debugDiagnostic = s"(internal diagnostic: $internalMessage)"
    val message = if (macroDebugLite || macroDebugVerbose) s"$friendlyMessage $debugDiagnostic" else friendlyMessage
    // TODO: clean this up! (This is a more explicit version of what the code use to do, to reveal the issue.)
    throw new TypeError(analyzer.lastTreeToTyper.pos, message)
  }

  def MacroCantExpand210xMacrosError(internalMessage: String) =
    MacroIncompatibleEngineError("can't expand macros compiled by previous versions of Scala", internalMessage)

  def MacroCantExpandIncompatibleMacrosError(internalMessage: String) =
    MacroIncompatibleEngineError("macro cannot be expanded, because it was compiled by an incompatible macro engine", internalMessage)

  /** The implicit not found message from the annotation, and whether it's a supplement message or not. */
  def NoImplicitFoundAnnotation(tree: Tree, param: Symbol): (Boolean, String) =
    param match {
      case ImplicitNotFoundMsg(msg) => (false, msg.formatParameterMessage(tree))
      case _ =>
        val paramTp = param.tpe
        paramTp.typeSymbolDirect match {
          case ImplicitNotFoundMsg(msg) => (false, msg.formatDefSiteMessage(paramTp))
          case _ =>
            val supplement = param.baseClasses.collectFirst {
              case ImplicitNotFoundMsg(msg) => s" (${msg.formatDefSiteMessage(paramTp)})"
            }.getOrElse("")
            true -> supplement
        }
    }

  def NoImplicitFoundError(tree: Tree, param: Symbol)(implicit context: Context): Unit = {
    val (isSupplement, annotationMsg) = NoImplicitFoundAnnotation(tree, param)
    def defaultErrMsg = {
      val paramName = param.name
      val paramTp = param.tpe
      def evOrParam =
        if (paramName startsWith nme.EVIDENCE_PARAM_PREFIX)
          "evidence parameter of type"
        else
          s"parameter $paramName:"
      if (isSupplement) s"could not find implicit value for $evOrParam $paramTp$annotationMsg"
      else annotationMsg
    }
    val errMsg = splainPushOrReportNotFound(tree, param, annotationMsg)
    issueNormalTypeError(tree, if (errMsg.isEmpty) defaultErrMsg else errMsg)
  }

  private def InferredImplicitErrorImpl(tree: Tree, inferred: Type, cx: Context, isTyper: Boolean): Unit = {
    val sym = tree.symbol
    def err(): Unit = {
      val msg =
        s"Implicit definition ${if (currentRun.isScala3) "must" else "should"} have explicit type${
          if (!inferred.isErroneous) s" (inferred $inferred)" else ""
        }"
      val namePos = if (sym.isAccessor && sym.accessed.pos.isDefined) sym.accessed.pos else sym.pos  //tree.asInstanceOf[NameTree].namePos
      val src = namePos.source
      val pos = if (src.sourceAt(namePos) != tree.symbol.decodedName) None else {
        val declEnd =
          if (sym.isAccessor) namePos.end
          else {
            val vdd = tree.asInstanceOf[ValOrDefDef]
            val eql = src.indexWhere(_ == '=', start = vdd.rhs.pos.start, step = -1)
            src.indexWhere(!_.isWhitespace, start = eql - 1, step = -1) + 1
          }
        Some(declEnd).filter(_ > 0).map(src.position(_))
      }
      val action = pos.map(p => runReporting.codeAction("insert explicit type", p, s": $inferred", msg)).getOrElse(Nil)
      if (currentRun.isScala3) cx.warning(tree.pos, msg, WarningCategory.Scala3Migration, action)
      else cx.warning(tree.pos, msg, WarningCategory.OtherImplicitType, action)
    }
    // Defer warning field of class until typing getter (which is marked implicit)
    if (sym.isImplicit) {
      if (!sym.isLocalToBlock) err()
    }
    else if (!isTyper && sym.isField && !sym.isLocalToBlock)
      sym.updateAttachment(FieldTypeInferred)
  }

  trait TyperContextErrors {
    self: Typer =>

    import infer.setError

    object TyperErrorGen {
      implicit val contextTyperErrorGen: Context = infer.getContext

      def UnstableTreeError(tree: Tree): tree.type = {
        def addendum = {
          "\n Note that "+tree.symbol+" is not stable because its type, "+tree.tpe+", is volatile."
        }
        issueNormalTypeError(tree,
          "stable identifier required, but "+tree+" found." + (
          if (treeInfo.hasVolatileType(tree)) addendum else ""))
        setError(tree)
      }

      def AdaptTypeError(tree: Tree, found: Type, req: Type): Tree = {
        // scala/bug#3971 unwrapping to the outermost Apply helps prevent confusion with the
        // error message point.
        def callee = {
          @tailrec
          def unwrap(t: Tree): Tree = t match {
            case Apply(app: Apply, _) => unwrap(app)
            case _                    => t
          }
          unwrap(tree)
        }

        def issueError(foundType: Type): Tree = {
          assert(!foundType.isErroneous, s"AdaptTypeError - foundType is Erroneous: $foundType")
          assert(!req.isErroneous, s"AdaptTypeError - req is Erroneous: $req")
          issueNormalTypeError(callee, withAddendum(callee.pos)(typeErrorMsg(context, foundType, req)))
          infer.explainTypes(foundType, req)
          setError(tree)
        }

        // If the expected type is a refinement type, and the found type is a refinement or an anon
        // class, we can greatly improve the error message by retyping the tree to recover the actual
        // members present, then display along with the expected members. This is done here because
        // this is the last point where we still have access to the original tree, rather than just
        // the found/req types.
        req.dealiasWiden match {
          case RefinedType(parents, decls) if !decls.isEmpty && found.typeSymbol.isAnonOrRefinementClass =>
            val retyped    = typed (tree.duplicate.clearType())
            val foundDecls = retyped.tpe.decls filter (sym => !sym.isConstructor && !sym.isSynthetic)
            if (foundDecls.isEmpty || (found.typeSymbol eq NoSymbol)) issueError(found)
            else {
              // The members arrive marked private, presumably because there was no
              // expected type and so they're considered members of an anon class.
              foundDecls foreach (_.makePublic)
              // TODO: if any of the found parents match up with required parents after normalization,
              // print the error so that they match. The major beneficiary there would be
              // java.lang.Object vs. AnyRef.
              val refined = refinedType(found.parents, found.typeSymbol.owner, foundDecls, tree.pos)
              // If the refinement type of an anonymous class is erroneous, the errors will be issued at its definition.
              if (found.typeSymbol.isAnonymousClass && refined.isErroneous) tree else issueError(refined)
            }
          case _ =>
            issueError(found)
        }
      }

      def WithFilterError(tree: Tree, ex: AbsTypeError) = {
        issueTypeError(ex)
        setError(tree)
      }

      def ParentTypesError(templ: Template, ex: TypeError) = {
        templ.clearType()
        issueNormalTypeError(templ, ex.getMessage())
        setError(templ)
      }

      def AuxConstrInConstantAnnotation(constr: Tree, clazz: Symbol) =
        issueNormalTypeError(constr, s"$clazz cannot have auxiliary constructors because it extends ConstantAnnotation")

      def ConstantAnnotationNeedsSingleArgumentList(constr: Tree, clazz: Symbol) =
        issueNormalTypeError(constr, s"$clazz needs to have exactly one argument list because it extends ConstantAnnotation")


      private def formatTraitWithParams(parent: Symbol, paramSyms: List[Symbol]): String = {
        val params = paramSyms.map(param => s"${param.name}: ${param.info}").mkString("(", ", ", ")")
        s"$parent$params"
      }

      // additional parentTypes errors
      def ConstrArgsInParentWhichIsTraitError(arg: Tree, parent: Symbol) = {
        val msg = parent.attachments.get[DottyParameterisedTrait] match {
          case Some(holder) =>
            val prettyParent = formatTraitWithParams(parent, holder.params)
            s"$prettyParent is an illegal Scala 3 parameterized trait; so can not take constructor arguments"
          case none => s"$parent is a trait; does not take constructor arguments"

        }
        issueNormalTypeError(arg, msg)
      }

      def ConstrArgsInParentOfTraitError(arg: Tree, parent: Symbol) =
        issueNormalTypeError(arg, "parents of traits may not have parameters")

      def MissingTypeArgumentsParentTpeError(supertpt: Tree) =
        issueNormalTypeError(supertpt, "missing type arguments")

      // typedIdent
      def AmbiguousIdentError(tree: Tree, name: Name, msg: String) =
        NormalTypeError(tree, "reference to " + name + " is ambiguous;\n" + msg)

      def SymbolNotFoundError(tree: Tree, name: Name, owner: Symbol, startingIdentCx: Context, inPattern: Boolean) = {
        def help = if (inPattern && name.isTermName) s"\nIdentifiers ${if (name.charAt(0).isUpper) "that begin with uppercase" else "enclosed in backticks"} are not pattern variables but match the value in scope." else ""
        NormalTypeError(tree, s"not found: ${decodeWithKind(name, owner)}$help")
      }

      // typedAppliedTypeTree
      def AppliedTypeNoParametersError(tree: Tree, errTpe: Type) = {
        issueNormalTypeError(tree, s"$errTpe does not take type parameters")
        setError(tree)
      }

      def AppliedTypeWrongNumberOfArgsError(tree: Tree, tpt: Tree, tparams: List[Symbol]) = {
        val tptSafeString: String = try {
          tpt.tpe.toString()
        } catch {
          case _: CyclicReference =>
            tpt.toString()
        }
        val msg = "wrong number of type arguments for "+tptSafeString+", should be "+tparams.length
        issueNormalTypeError(tree, msg)
        setError(tree)
      }

      // typedTypeDef
      def LowerBoundError(tree: TypeDef, lowB: Type, highB: Type) =
        issueNormalTypeError(tree, "lower bound "+lowB+" does not conform to upper bound "+highB)

      def HiddenSymbolWithError[T <: Tree](tree: T): T =
        setError(tree)

      def SymbolEscapesScopeError[T <: Tree](tree: T, badSymbol: Symbol): T = {
        val modifierString = if (badSymbol.isPrivate) "private " else ""
        issueNormalTypeError(tree, modifierString + badSymbol + " escapes its defining scope as part of type "+tree.tpe)
        setError(tree)
      }

      // typedDefDef
      def StarParamNotLastError(param: Tree) =
        issueNormalTypeError(param, "*-parameter must come last")

      def StarWithDefaultError(meth: Symbol) =
        issueSymbolTypeError(meth, "a parameter section with a `*`-parameter is not allowed to have default arguments")

      def InvalidConstructorDefError(ddef: Tree) =
        issueNormalTypeError(ddef, "constructor definition not allowed here")

      def DeprecatedParamNameError(param: Symbol, name: Name) =
        issueSymbolTypeError(param, s"deprecated parameter name $name has to be distinct from any other parameter name (deprecated or not).")

      // analyzeSuperConsructor
      def SuperConstrReferenceError(tree: Tree) =
        NormalTypeError(tree, "super constructor cannot be passed a self reference unless parameter is declared by-name")

      def SuperConstrArgsThisReferenceError(tree: Tree) =
        ConstrArgsThisReferenceError("super", tree)

      def SelfConstrArgsThisReferenceError(tree: Tree) =
        ConstrArgsThisReferenceError("self", tree)

      private def ConstrArgsThisReferenceError(prefix: String, tree: Tree) =
        NormalTypeError(tree, s"$prefix constructor arguments cannot reference unconstructed `this`")

      def TooManyArgumentListsForConstructor(tree: Tree) = {
        issueNormalTypeError(tree, "too many argument lists for constructor invocation")
        setError(tree)
      }

      // typedValDef
      def VolatileValueError(vdef: Tree) =
        issueNormalTypeError(vdef, "values cannot be volatile")

      def LocalVarUninitializedError(vdef: Tree) =
        issueNormalTypeError(vdef, "local variables must be initialized")

      //typedAssign
      def AssignmentError(tree: Tree, varSym: Symbol) = {
        issueNormalTypeError(tree,
          if (varSym != null && varSym.isValue) "reassignment to val"
          else "assignment to non variable")
        setError(tree)
      }

      def UnexpectedTreeAssignmentConversionError(tree: Tree) = {
        issueNormalTypeError(tree, "Unexpected tree during assignment conversion.")
        setError(tree)
      }

      //typedSuper
      def MixinMissingParentClassNameError(tree: Tree, mix: Name, clazz: Symbol) =
        issueNormalTypeError(tree, s"$mix does not name a parent class of $clazz")

      def AmbiguousParentClassError(tree: Tree) =
        issueNormalTypeError(tree, "ambiguous parent class qualifier")

      //typedSelect
      def NotAMemberError(sel: Tree, qual: Tree, name: Name, cx: Context) = {
        import util.EditDistance, util.StringUtil.oxford
        def errMsg: String = {
          val editThreshold  = 3
          val maxSuggestions = 4

          val owner            = qual.tpe.typeSymbol
          val target           = qual.tpe.widen
          def targetKindString = if (owner.isTypeParameterOrSkolem) "type parameter " else ""
          def nameString       = decodeWithKind(name, owner)
          /* Illuminating some common situations and errors a bit further. */
          def addendum         = {
            @inline def orEmpty(cond: Boolean)(s: => String) = if (cond) s else ""
            val companionSymbol: Symbol = {
              if (name.isTermName && owner.isPackageClass)
                target.member(name.toTypeName)
              else NoSymbol
            }
            val companion = orEmpty(companionSymbol != NoSymbol)(s"note: $companionSymbol exists, but it has no companion object.")
            // find out all the names available under target within ~2 edit distances
            lazy val alternatives: List[(Int, String)] = {
              val x = name.decode
              // effectively suppress comparison ops, but if they say <= and there is >=, offer it
              def isEncodedComparison(n: Name) = n match {
                case nme.EQ | nme.NE | nme.LT | nme.GT | nme.LE | nme.GE => true
                case _ => false
              }
              val nameIsComparison = isEncodedComparison(name)
              if (context.openImplicits.nonEmpty || x.length < 2) Nil
              else {
                target.members.iterator
                  .filter(sym => sym.isTerm == name.isTermName &&
                    !sym.isConstructor &&
                    !nme.isLocalName(sym.name) &&
                    isEncodedComparison(sym.name) == nameIsComparison &&
                    sym.name != nme.EQ && sym.name != nme.NE &&
                    cx.isAccessible(sym, target))
                  .map(_.name.decode)
                  .filter { n =>
                    math.abs(n.length - x.length) <= editThreshold &&
                    n != x &&
                    !n.contains("$")
                  }
                  .map(n => (EditDistance.levenshtein(n, x), n))
                  .filter { case (d, n) =>
                    val nset = n.endsWith("_=")
                    val xset = x.endsWith("_=")
                    val (n1, x1) = if (nset && xset) (n.dropRight(2), x.dropRight(2)) else (n, x)
                    def contained = x1.forall(c => n1.indexOf(c) >= 0)
                    !(nset ^ xset) && d <= editThreshold && (d <= n1.length/2 && d <= x1.length/2 || contained)
                  }
                  .toList.sorted
              }
            }
            val altStr: String =
              orEmpty(companionSymbol == NoSymbol && alternatives.nonEmpty) {
                val d0 = alternatives.head._1
                val (best0, rest0) = alternatives.span(_._1 == d0)
                val best = best0.map(_._2).distinct
                val rest = rest0.map(_._2).distinct
                val more = (maxSuggestions - best.length) max 0
                val add1 = orEmpty(more > 0 && rest.nonEmpty)(s" or perhaps ${oxford(rest.take(more), "or")}?")
                val add2 = orEmpty(best.length > maxSuggestions || rest.length > more)(" or...?")
                s"did you mean ${oxford(best.take(maxSuggestions), "or")}?$add1$add2"
              }
            val semicolon = orEmpty(linePrecedes(qual, sel))(s"possible cause: maybe a semicolon is missing before `$nameString`?")
            val notAnyRef = orEmpty(ObjectClass.info.member(name).exists)(notAnyRefMessage(target))
            val javaRules = orEmpty(owner.isClass && !owner.hasPackageFlag) {
              owner.baseClasses.iterator.filter(bc => bc.ne(ObjectClass) && bc.isJavaDefined)
                .map { bc => cx.javaFindMember(bc.info, name, _.isStaticMember) match {
                    case (_, NoSymbol) if name.isTermName =>
                      cx.javaFindMember(bc.info, name.toTypeName, _.isStaticMember)
                    case res => res
                  }
                }
                .find(_._2 ne NoSymbol) match {
                  case Some((jtype, jmember)) =>
                    val more = sm"""Static Java members belong to companion objects in Scala;
                      |they are not inherited, even by subclasses defined in Java."""
                    s"did you mean ${jtype.typeSymbol.fullName}.${jmember.name}? $more"
                  case _ => ""
                }
            }
            List(companion, altStr, notAnyRef, semicolon, javaRules).filter("" != _).map("\n" + _).mkString
          }
          def targetStr = targetKindString + target.directObjectString
          withAddendum(qual.pos)(
            if (name == nme.CONSTRUCTOR) s"$target does not have a constructor"
            else s"$nameString is not a member of $targetStr$addendum"
          )
        }
        issueNormalTypeError(sel, errMsg)
        // the error has to be set for the copied tree, otherwise
        // the error remains persistent across multiple compilations
        // and causes problems
        //setError(sel)
      }

      def SelectWithUnderlyingError(sel: Tree, err: AbsTypeError): sel.type = {
        // if there's no position, this is likely the result of a MissingRequirementError
        // use the position of the selection we failed to type check to report the original message
        if (err.errPos == NoPosition) issueNormalTypeError(sel, err.errMsg)
        else issueTypeError(err)
        setError(sel)
      }

      //typedNew
      def IsAbstractError(tree: Tree, sym: Symbol) = {
        issueNormalTypeError(tree, s"$sym is abstract; cannot be instantiated")
        setError(tree)
      }

      def DoesNotExtendAnnotation(tree: Tree, sym: Symbol) = {
        NormalTypeError(tree, s"$sym does not extend ${AnnotationClass.fullName}")
      }

      def DoesNotConformToSelfTypeError(tree: Tree, sym: Symbol, tpe0: Type) = {
        issueNormalTypeError(tree, s"$sym cannot be instantiated because it does not conform to its self-type $tpe0")
        setError(tree)
      }

      def UnderscoreEtaError(tree: Tree) = {
        issueNormalTypeError(tree, "_ must follow method; cannot follow " + tree.tpe)
        setError(tree)
      }

      //typedReturn
      def ReturnOutsideOfDefError(tree: Tree) = {
        issueNormalTypeError(tree, "return outside method definition")
        setError(tree)
      }

      def ReturnWithoutTypeError(tree: Tree, owner: Symbol) = {
        issueNormalTypeError(tree, s"$owner has return statement; needs result type")
        setError(tree)
      }

      //typedBind
      def VariableInPatternAlternativeError(tree: Tree) = {
        issueNormalTypeError(tree, "illegal variable in pattern alternative")
        //setError(tree)
      }

      //typedCase
      def StarPositionInPatternError(tree: Tree) =
        issueNormalTypeError(tree, "_* may only come last")

      //typedFunction
      def MaxFunctionArityError(fun: Tree, why: String) = {
        issueNormalTypeError(fun, s"functions may not have more than ${definitions.MaxFunctionArity} parameters$why")
        setError(fun)
      }

      def MissingParameterTypeError(fun: Tree, vparam: ValDef, pt: Type, withTupleAddendum: Boolean) = {
        def issue(what: String) = {
          val addendum: String = fun match {
            case Function(params, _) if withTupleAddendum =>
              val funArity = params.length
              val example = analyzer.exampleTuplePattern(params map (_.name))
              (pt baseType FunctionClass(1)) match {
                case TypeRef(_, _, arg :: _) if arg.typeSymbol == TupleClass(funArity) && funArity > 1 =>
                  sm"""|
                       |Note: The expected type requires a one-argument function accepting a $funArity-Tuple.
                       |      Consider a pattern matching anonymous function, `{ case $example =>  ... }`"""
                case _ => ""
              }
            case _ => ""
          }
          issueNormalTypeError(vparam, what + addendum)
        }
        if (vparam.mods.isSynthetic) fun match {
          case Function(_, Match(_, _)) => MissingParameterTypeAnonMatchError(vparam, pt)
          case _                        => issue("missing parameter type for expanded function " + fun)
        } else issue("missing parameter type")
      }

      def MissingParameterTypeAnonMatchError(vparam: Tree, pt: Type) =
        issueNormalTypeError(vparam, "missing parameter type for expanded function\n"+
          "The argument types of an anonymous function must be fully known. (SLS 8.5)\n"+
          "Expected type was: " + pt.toLongString)

      def ConstructorsOrderError(tree: Tree) = {
        issueNormalTypeError(tree, "self constructor invocation must refer to a constructor definition which precedes it, to prevent infinite cycles")
        setError(tree)
      }

      def ConstructorRecursesError(tree: Tree) = {
        issueNormalTypeError(tree, "constructor invokes itself")
        setError(tree)
      }

      def OnlyDeclarationsError(tree: Tree) = {
        issueNormalTypeError(tree, "only declarations allowed here")
        setError(tree)
      }

      // typedAnnotation
      def AnnotationNotAConstantError(tree: Tree) =
        NormalTypeError(tree, "annotation argument needs to be a constant; found: " + tree)

      def AnnotationArgNullError(tree: Tree) =
        NormalTypeError(tree, "annotation argument cannot be null")

      def ArrayConstantsError(tree: Tree) =
        NormalTypeError(tree, "Array constants have to be specified using the `Array(...)` factory method")

      def ArrayConstantsTypeMismatchError(tree: Tree, pt: Type) =
        NormalTypeError(tree, "found array constant, expected argument of type " + pt)

      def AnnotationTypeMismatchError(tree: Tree, expected: Type, found: Type) =
        NormalTypeError(tree, "expected annotation of type " + expected + ", found " + found)

      def MultipleArgumentListForAnnotationError(tree: Tree) =
        NormalTypeError(tree, "multiple argument lists on Java annotation")

      def UnknownAnnotationNameError(tree: Tree, name: Name) =
        NormalTypeError(tree, "unknown annotation argument name: " + name)

      def DuplicateValueAnnotationError(tree: Tree, name: Name) =
        NormalTypeError(tree, "duplicate value for annotation argument " + name)

      def ClassfileAnnotationsAsNamedArgsError(tree: Tree) =
        NormalTypeError(tree, "arguments to Java annotations have to be supplied as named arguments")

      def AnnotationMissingArgError(tree: Tree, annType: Type, sym: Symbol) =
        NormalTypeError(tree, "annotation " + annType.typeSymbol.fullName + " is missing argument " + sym.name)

      def NestedAnnotationError(tree: Tree, annType: Type) =
        NormalTypeError(tree, "nested classfile annotations must be defined in java; found: "+ annType)

      def UnexpectedTreeAnnotationError(tree: Tree, unexpected: Tree) =
        NormalTypeError(tree, "unexpected tree after typing annotation: "+ unexpected)

      //typedExistentialTypeTree
      def AbstractionFromVolatileTypeError(vd: ValDef) =
        issueNormalTypeError(vd, "illegal abstraction from value with volatile type "+vd.symbol.tpe)

      private[scala] def TypedApplyWrongNumberOfTpeParametersErrorMessage(fun: Tree) =
        "wrong number of type parameters for "+treeSymTypeMsg(fun)

      def TypedApplyWrongNumberOfTpeParametersError(tree: Tree, fun: Tree) = {
        issueNormalTypeError(tree, TypedApplyWrongNumberOfTpeParametersErrorMessage(fun))
        setError(tree)
      }

      def TypedApplyDoesNotTakeTpeParametersError(tree: Tree, fun: Tree) = {
        issueNormalTypeError(tree, treeSymTypeMsg(fun)+" does not take type parameters.")
        setError(tree)
      }

      // doTypeApply
      //tryNamesDefaults
      def NamedAndDefaultArgumentsNotSupportedForMacros(tree: Tree, fun: Tree) =
        NormalTypeError(tree, "macro applications do not support named and/or default arguments")

      def TooManyArgsNamesDefaultsError(tree: Tree, fun: Tree, formals: List[Type], args: List[Tree], argPos: Array[Int]) = {
        val expected = formals.size
        val supplied = args.size
        // pick a caret. For f(k=1,i=2,j=3), argPos[0,-1,1] b/c `k=1` taken as arg0
        val excessive = {
          val i = argPos.indexWhere(_ >= expected)
          if (i < 0) tree else args(i min (supplied - 1))
        }
        val msg = {
          val target = treeSymTypeMsg(fun)
          def isAutoTuplable = AnyRefTpe <:< (if (formals.head.typeSymbol.isTypeParameter) formals.head.upperBound else formals.head)

          expected match {
            case 0 =>
              args match {
                case (c @ Literal(Constant(()))) :: Nil if c.hasAttachment[SyntheticUnitAttachment.type] =>
                  s"can't supply unit value with infix notation because nullary $target takes no arguments; use dotted invocation instead: ${show(treeCopy.Apply(tree, fun, Nil))}"
                case _ => s"no arguments allowed for nullary $target"
              }
            case 1 if isTupleType(formals.head) => s"too many arguments (found $supplied, expected ${formals.head.typeArgs.size}-tuple) for $target"
            case 1 if supplied > MaxTupleArity && isAutoTuplable => s"too many arguments (found $supplied, which exceeds the largest Tuple) for $target"
            case _ => s"too many arguments (found $supplied, expected $expected) for $target"
          }
        }
        NormalTypeError(excessive, msg)
      }

      // can it still happen? see test case neg/overloaded-unapply.scala
      def OverloadedUnapplyError(tree: Tree) =
        issueNormalTypeError(tree, "cannot resolve overloaded unapply")

      def UnapplyWithSingleArgError(tree: Tree) =
        issueNormalTypeError(tree, "an unapply method must accept a single argument.")

      def MultipleVarargError(tree: Tree) =
        NormalTypeError(tree, "when using named arguments, the vararg parameter has to be specified exactly once")

      def ModuleUsingCompanionClassDefaultArgsError(tree: Tree) =
        NormalTypeError(tree, "module extending its companion class cannot use default constructor arguments")

      def NotEnoughArgsError(tree: Tree, fun: Tree, missing: List[Symbol]) = {
        val notEnoughArgumentsMsg = {
          val suffix = if (missing.isEmpty) "" else {
            val keep = missing.take(3).map(_.name)
            val ess  = if (missing.tail.isEmpty) "" else "s"
            val dots = if (missing.drop(3).nonEmpty) "..." else "."
            keep.mkString(s".\nUnspecified value parameter$ess ", ", ", dots)
          }
          s"not enough arguments for ${ treeSymTypeMsg(fun) }$suffix"
        }
        NormalTypeError(tree, notEnoughArgumentsMsg)
      }

      //doTypedApply - patternMode
      def TooManyArgsPatternError(fun: Tree) =
        issueNormalTypeError(fun, "too many arguments for unapply pattern, maximum = "+definitions.MaxTupleArity)

      def BlackboxExtractorExpansion(fun: Tree) =
        issueNormalTypeError(fun, "extractor macros can only be whitebox")

      def WrongShapeExtractorExpansion(fun: Tree) =
        issueNormalTypeError(fun, "extractor macros can only expand into extractor calls")

      def WrongNumberOfArgsError(tree: Tree, fun: Tree) =
        NormalTypeError(tree, "wrong number of arguments for "+ treeSymTypeMsg(fun))

      def ApplyWithoutArgsError(tree: Tree, fun: Tree) =
        NormalTypeError(tree, s"${fun.tpe} does not take parameters")

      // Dynamic
      def DynamicVarArgUnsupported(tree: Tree, name: Name) = {
        issueNormalTypeError(tree, s"$name does not support passing a vararg parameter")
        setError(tree)
      }

      def DynamicRewriteError(tree: Tree, err: AbsTypeError) = {
        issueTypeError(PosAndMsgTypeError(err.errPos, err.errMsg +
            s"\nerror after rewriting to $tree\npossible cause: maybe a wrong Dynamic method signature?"))
        setError(tree)
      }

      //checkClassType
      def TypeNotAStablePrefixError(tpt: Tree, pre: Type): tpt.type = {
        issueNormalTypeError(tpt, "type "+pre+" is not a stable prefix")
        setError(tpt)
      }

      def ClassTypeRequiredError(tree: Tree, found: AnyRef): tree.type = {
        issueNormalTypeError(tree, "class type required but "+found+" found")
        setError(tree)
      }

      // validateParentClasses
      def ParentSuperSubclassError(parent: Tree, superclazz: Symbol,
                 parentSym: Symbol, mixin: Symbol) =
        NormalTypeError(parent, "illegal inheritance; super"+superclazz+
                   "\n is not a subclass of the super"+parentSym+
                   "\n of the mixin " + mixin)

      def ParentIsScala3TraitError(parent: Tree,
                 parentSym: Symbol, params: List[Symbol], mixin: Symbol) = {
        val parentWithCtor = formatTraitWithParams(parentSym, params)
        val mixinMsg = {
          if (mixin eq parentSym) " parameterized mixin "+mixin
          else " parameterized super"+parentSym+"\n of the mixin "+mixin
        }
        NormalTypeError(parent, "illegal inheritance;"+mixinMsg+
                  "\n is defined in Scala 3 as " + parentWithCtor)
        }

      def ParentNotATraitMixinError(parent: Tree, mixin: Symbol) =
        NormalTypeError(parent, s"$mixin needs to be a trait to be mixed in")

      def ParentFinalInheritanceError(parent: Tree, mixin: Symbol) =
        NormalTypeError(parent, s"illegal inheritance from final $mixin")

      def ParentSelfTypeConformanceError(parent: Tree, selfType: Type) =
        NormalTypeError(parent,
          "illegal inheritance;\n self-type "+selfType+" does not conform to "+
          parent +"'s selftype "+parent.tpe.typeOfThis)

      def ParentInheritedTwiceError(parent: Tree, parentSym: Symbol) =
        NormalTypeError(parent, s"$parentSym is inherited twice")

      //adapt
      def MissingArgsForMethodTpeError(tree: Tree, meth: Symbol) = {
        val f = meth.name.decoded
        val paf = s"$f(${ meth.asMethod.paramLists map (_ map (_ => "_") mkString ",") mkString ")(" })"
        val advice =
          if (meth.isConstructor || meth.info.params.lengthIs > definitions.MaxFunctionArity) ""
          else s"""
            |Unapplied methods are only converted to functions when a function type is expected.
            |You can make this conversion explicit by writing `$f _` or `$paf` instead of `$f`.""".stripMargin
        val message =
          if (meth.isMacro) MacroTooFewArgumentListsMessage
          else s"""missing argument list for ${meth.fullLocationString}$advice"""
        issueNormalTypeError(tree, message)
        setError(tree)
      }

      def MissingTypeParametersError(tree: Tree) = {
        issueNormalTypeError(tree, s"${tree.symbol} takes type parameters")
        setError(tree)
      }

      def KindArityMismatchError(tree: Tree, pt: Type) = {
        issueNormalTypeError(tree,
          s"${tree.tpe} takes ${countElementsAsString(tree.tpe.typeParams.length, "type parameter")}, expected: ${countAsString(pt.typeParams.length)}")
        setError(tree)
      }

      def CaseClassConstructorError(tree: Tree, baseMessage: String) = {
        import UnapplyMemberResult._
        val addendum = {
          def contextualize(sym: Symbol, because: String) =
            s"\nNote: ${sym.defString} exists in ${tree.symbol}, but it cannot be used as an extractor$because"
          val sym = directUnapplyMember(tree.symbol.info)
          validateUnapplyMember(sym.info) match {
            case NoParams =>
              contextualize(sym, ": an unapply method must accept a single argument")
            case MultiParams =>
              contextualize(sym, " as it has more than one (non-implicit) parameter")
            case MultiParamss =>
              contextualize(sym, " due to its second non-implicit parameter list")
            case VarArgs =>
              contextualize(sym, " since it is a varargs method")
            case _ => ""
          }
        }
        issueNormalTypeError(tree, baseMessage + addendum)
        setError(tree)
      }

      def ConstructorPrefixError(tree: Tree, restpe: Type) = {
        issueNormalTypeError(tree, s"${restpe.prefix} is not a legal prefix for a constructor")
        setError(tree)
      }

      // typedPattern
      def PatternMustBeValue(pat: Tree, pt: Type) =
        issueNormalTypeError(pat, s"pattern must be a value: $pat"+ typePatternAdvice(pat.tpe.typeSymbol, pt.typeSymbol))

      // SelectFromTypeTree
      def TypeSelectionFromVolatileTypeError(tree: Tree, qual: Tree) = {
        val hiBound = qual.tpe.upperBound
        val addendum = if (hiBound =:= qual.tpe) "" else s" (with upper bound ${hiBound})"
        issueNormalTypeError(tree, s"illegal type selection from volatile type ${qual.tpe}${addendum}")
        setError(tree)
      }

      // packedType
      def InferTypeWithVolatileTypeSelectionError(tree: Tree, pre: Type) =
        issueNormalTypeError(tree, "Inferred type "+tree.tpe+" contains type selection from volatile type "+pre)

      def AbstractExistentiallyOverParamerizedTpeError(tree: Tree, tp: Type) =
        issueNormalTypeError(tree, "can't existentially abstract over parameterized type " + tp)

      // resolveClassTag
      def MissingClassTagError(tree: Tree, tp: Type) = {
        issueNormalTypeError(tree, "cannot find class tag for element type "+tp)
        setError(tree)
      }

      // cases where we do not necessarily return trees

      //checkStarPatOK
      def StarPatternWithVarargParametersError(tree: Tree) =
        issueNormalTypeError(tree, "star patterns must correspond with varargs parameters")

      def FinitaryError(tparam: Symbol) =
        issueSymbolTypeError(tparam, "class graph is not finitary because type parameter "+tparam.name+" is expansively recursive")

      def QualifyingClassError(tree: Tree, qual: Name) =
        issueNormalTypeError(tree,
          if (qual.isEmpty) s"$tree can be used only in a class, object, or template"
          else s"$qual is not an enclosing class")

      // def stabilize
      def NotAValueError(tree: Tree, sym: Symbol) = {
        issueNormalTypeError(tree, s"${sym.kindString} ${sym.fullName} is not a value")
        setError(tree)
      }

      def DefDefinedTwiceError(sym0: Symbol, sym1: Symbol) = {
        val addPref = s";\n  the conflicting $sym1 was defined"
        val bugNote = "\n  Note: this may be due to a bug in the compiler involving wildcards in package objects"

        // Most of this hard work is associated with scala/bug#4893.
        val isBug = sym0.isAbstractType && sym1.isAbstractType && (sym0.name startsWith "_$")
        val addendum = (
          if (sym0.pos.source eq sym1.pos.source)   s"$addPref at line ${sym1.pos.line}:${sym1.pos.column}"
          else if (sym1.pos.source ne NoSourceFile) s"$addPref at line ${sym1.pos.line}:${sym1.pos.column} of '${sym1.pos.source.path}'"
          else if (sym1.associatedFile ne NoAbstractFile) s"$addPref in '${sym1.associatedFile.canonicalPath}'"
          else "") + (if (isBug) bugNote else "")

        issueSymbolTypeError(sym0, s"$sym0 is defined twice$addendum")
      }

      // cyclic errors

      def CyclicAliasingOrSubtypingError(errPos: Position, sym0: Symbol) =
        issueTypeError(PosAndMsgTypeError(errPos, "cyclic aliasing or subtyping involving "+sym0))

      // macro-related errors (also see MacroErrors below)

      def MacroEtaError(tree: Tree) = {
        issueNormalTypeError(tree, "macros cannot be eta-expanded")
        setError(tree)
      }

      def MacroTooManyArgumentListsError(expandee: Tree, fun: Symbol) = {
        NormalTypeError(expandee, "too many argument lists for " + fun)
      }


      case object MacroExpansionException extends ControlThrowable

      protected def macroExpansionError(expandee: Tree, msg: String, pos: Position = NoPosition) = {
        def msgForLog = if (msg != null && (msg contains "exception during macro expansion")) msg.split(EOL).drop(1).headOption.getOrElse("?") else msg
        macroLogLite("macro expansion has failed: %s".format(msgForLog))
        if (msg != null) context.error(if (pos.isDefined) pos else expandee.pos, msg) // issueTypeError(PosAndMsgTypeError(..)) won't work => swallows positions
        setError(expandee)
        throw MacroExpansionException
      }

      private def macroExpansionError2(expandee: Tree, msg: String) = {
        // macroExpansionError won't work => swallows positions, hence needed to do issueTypeError
        // kinda contradictory to the comment in `macroExpansionError`, but this is how it works
        issueNormalTypeError(expandee, msg)
        setError(expandee)
        throw MacroExpansionException
      }

      private def MacroTooFewArgumentListsMessage = "too few argument lists for macro invocation"
      def MacroTooFewArgumentListsError(expandee: Tree) = macroExpansionError2(expandee, MacroTooFewArgumentListsMessage)

      private def MacroTooManyArgumentListsMessage = "too many argument lists for macro invocation"
      def MacroTooManyArgumentListsError(expandee: Tree) = macroExpansionError2(expandee, MacroTooManyArgumentListsMessage)

      def MacroTooFewArgumentsError(expandee: Tree) = macroExpansionError2(expandee, "too few arguments for macro invocation")

      def MacroTooManyArgumentsError(expandee: Tree) = macroExpansionError2(expandee, "too many arguments for macro invocation")

      def MacroGeneratedAbort(expandee: Tree, ex: AbortMacroException) = {
        // errors have been reported by the macro itself, so we do nothing here
        macroLogVerbose("macro expansion has been aborted")
        macroExpansionError(expandee, ex.msg, ex.pos)
      }

      def MacroGeneratedTypeError(expandee: Tree, err: TypeError = null) =
        if (err == null) {
          // errors have been reported by the macro itself, so we do nothing here
          macroExpansionError(expandee, null)
        } else {
          macroLogLite("macro expansion has failed: %s at %s".format(err.msg, err.pos))
          throw err // this error must be propagated, don't report
        }

      def MacroGeneratedException(expandee: Tree, ex: Throwable) = {
        val realex = ReflectionUtils.unwrapThrowable(ex)
        val message = {
          try {
            val relevancyThreshold = realex.getStackTrace().indexWhere(_.getMethodName.endsWith("macroExpandWithRuntime"))
            if (relevancyThreshold == -1) None
            else {
              val relevantElements = realex.getStackTrace().take(relevancyThreshold - 1)
              def isMacroInvoker(este: StackTraceElement) = este.getMethodName.startsWith("invoke")
              val keep = relevantElements.reverse.dropWhile(isMacroInvoker).reverse
              realex.setStackTrace(keep)
              Some(EOL + stackTraceString(realex))
            }
          } catch {
            // the code above tries various tricks to detect the relevant portion of the stack trace
            // if these tricks fail, just fall back to uninformative, but better than nothing.
            case NonFatal(ex) =>
              macroLogVerbose("got an exception when processing a macro generated exception\n" +
                              "offender = " + stackTraceString(realex) + "\n" +
                              "error = " + stackTraceString(ex))
              None
          }
        } getOrElse {
          val msg = realex.getMessage
          if (msg != null) msg else realex.getClass.getName
        }
        macroExpansionError(expandee, "exception during macro expansion: " + message)
      }

      def MacroFreeSymbolError(expandee: Tree, sym: FreeSymbol) = {
        val kind = sym.name.nameKind
        val name = s"${sym.name} ${sym.origin}"
        val forgotten = if (sym.isTerm) "splice when splicing this variable into a reifee" else "c.WeakTypeTag annotation for this type parameter"
        macroExpansionError(expandee,
          s"Macro expansion contains free $kind variable $name. Have you forgotten to use $forgotten? If you have troubles tracking free $kind variables, consider using -Xlog-free-${kind}s"
        )
      }

      def MacroExpansionHasInvalidTypeError(expandee: Tree, expanded: Any) = {
        def isUnaffiliatedExpr = expanded.isInstanceOf[scala.reflect.api.Exprs#Expr[_]]
        def isUnaffiliatedTree = expanded.isInstanceOf[scala.reflect.api.Trees#TreeApi]
        val expected = "expr or tree"
        val actual = if (isUnaffiliatedExpr) "an expr" else if (isUnaffiliatedTree) "a tree" else "unexpected"
        val isPathMismatch = expanded != null && (isUnaffiliatedExpr || isUnaffiliatedTree)
        macroExpansionError(expandee,
          s"macro must return a compiler-specific $expected; returned value is " + (
            if (expanded == null) "null"
            else if (isPathMismatch) s"$actual, but it doesn't belong to this compiler's universe"
            else s"of ${expanded.getClass}"
        ))
      }

      def MacroImplementationNotFoundError(expandee: Tree) =
        macroExpansionError(expandee, macroImplementationNotFoundMessage(expandee.symbol.name))

      def MacroAnnotationShapeError(clazz: Symbol) = {
        val sym = clazz.info.member(nme.macroTransform)
        var actualSignature = sym.toString
        if (sym.isOverloaded) actualSignature += "(...) = ..."
        else if (sym.isMethod) {
          if (sym.typeParams.nonEmpty) {
            def showTparam(tparam: Symbol) =
              tparam.typeSignature match {
                case tpe @ TypeBounds(_, _) => s"${tparam.name}$tpe"
                case _ => tparam.name
              }
            def showTparams(tparams: List[Symbol]) = "[" + (tparams map showTparam mkString ", ") + "]"
            actualSignature += showTparams(sym.typeParams)
          }
          if (sym.paramss.nonEmpty) {
            def showParam(param: Symbol) = s"${param.name}: ${param.typeSignature}"
            def showParams(params: List[Symbol]) = {
              val s_mods = if (params.nonEmpty && params(0).hasFlag(scala.reflect.internal.Flags.IMPLICIT)) "implicit " else ""
              val s_params = params map showParam mkString ", "
              "(" + s_mods + s_params + ")"
            }
            def showParamss(paramss: List[List[Symbol]]) = paramss map showParams mkString ""
            actualSignature += showParamss(sym.paramss)
          }
          if (sym.isTermMacro) actualSignature = actualSignature.replace("macro method", "def") + " = macro ..."
          else actualSignature = actualSignature.replace("method", "def") + " = ..."
        }
        issueSymbolTypeError(clazz, s"""
                                       |macro annotation has wrong shape:
                                       |  required: def macroTransform(annottees: Any*) = macro ...
                                       |  found   : $actualSignature
      """.trim.stripMargin)
      }

      def MacroAnnotationMustBeStaticError(clazz: Symbol) =
        issueSymbolTypeError(clazz, "macro annotation must extend scala.annotation.StaticAnnotation")

      def MacroAnnotationCannotBeInheritedError(clazz: Symbol) =
        issueSymbolTypeError(clazz, "macro annotation cannot be @Inherited")

      def MacroAnnotationCannotBeMemberError(clazz: Symbol) =
        issueSymbolTypeError(clazz, "macro annotation cannot be a member of another class")

      def MacroAnnotationNotExpandedMessage: String =
        "macro annotation could not be expanded (since these are experimental, you must enable them with -Ymacro-annotations)"

      def MacroAnnotationOnlyDefinitionError(ann: Tree) =
        issueNormalTypeError(ann, "macro annotations can only be put on definitions")

      def MacroAnnotationTopLevelClassWithCompanionBadExpansion(ann: Tree) =
        issueNormalTypeError(ann, "top-level class with companion can only expand into a block consisting in eponymous companions")

      def MacroAnnotationTopLevelClassWithoutCompanionBadExpansion(ann: Tree) =
        issueNormalTypeError(ann, "top-level class without companion can only expand either into an eponymous class or into a block consisting in eponymous companions")

      def MacroAnnotationTopLevelModuleBadExpansion(ann: Tree) =
        issueNormalTypeError(ann, "top-level object can only expand into an eponymous object")

      def InferredImplicitError(tree: Tree, inferred: Type, cx: Context): Unit =
        InferredImplicitErrorImpl(tree, inferred, cx, isTyper = true)
    }

    /** This file will be the death of me. */
    protected def macroImplementationNotFoundMessage(name: Name): String = (
      s"""|macro implementation not found: $name
          |(the most common reason for that is that you cannot use macro implementations in the same compilation run that defines them)""".stripMargin
    )
  }

  trait InferencerContextErrors {
    self: Inferencer =>

    private def applyErrorMsg(tree: Tree, msg: String, argtpes: List[Type], pt: Type) = {
      def asParams(xs: List[Any]) =
        if (xs.isEmpty && tree.symbol.isConstructor) "no arguments"
        else xs.mkString("(", ", ", ")")

      def resType   = if (pt.isWildcard) "" else " with expected result type " + pt
      def allTypes  = (alternatives(tree) flatMap (_.paramTypes)) ++ argtpes :+ pt
      def locals    = alternatives(tree) flatMap (_.typeParams)

      withDisambiguation(locals, allTypes: _*) {
        treeSymTypeMsg(tree) + msg + asParams(argtpes) + resType
      }
    }

    object InferErrorGen {

      implicit val contextInferErrorGen: Context = getContext

      object PolyAlternativeErrorKind extends Enumeration {
        type ErrorType = Value
        val WrongNumber, NoParams, ArgsDoNotConform = Value
      }

      private def issueAmbiguousTypeErrorUnlessErroneous(pos: Position, pre: Type, sym1: Symbol, sym2: Symbol, rest: String): Unit = {
        // To avoid stack overflows (scala/bug#8890), we MUST (at least) report when either `validTargets` OR `ambiguousSuppressed`
        // More details:
        // If `!context.ambiguousErrors`, `reporter.issueAmbiguousError` (which `context.issueAmbiguousError` forwards to)
        // buffers ambiguous errors. In this case, to avoid looping, we must issue even if `!validTargets`. (TODO: why?)
        // When not buffering (and thus reporting to the user), we shouldn't issue unless `validTargets`,
        // otherwise we report two different errors that trace back to the same root cause,
        // and unless `validTargets`, we don't know for sure the ambiguity is real anyway.
        val validTargets = !(pre.isErroneous || sym1.isErroneous || sym2.isErroneous)
        val ambiguousBuffered = !context.ambiguousErrors
        if (validTargets || ambiguousBuffered)
          context.issueAmbiguousError(
            if (sym1.hasDefault && sym2.hasDefault && sym1.enclClass == sym2.enclClass) {
              val methodName = nme.defaultGetterToMethod(sym1.name)
              AmbiguousTypeError(sym1.enclClass.pos,
                s"in ${sym1.enclClass}, multiple overloaded alternatives of $methodName define default arguments")

            } else {
              AmbiguousTypeError(pos,
                 "ambiguous reference to overloaded definition,\n" +
                s"both ${sym1.fullLocationString} of type ${pre.memberType(sym1)}\n" +
                s"and  ${sym2.fullLocationString} of type ${pre.memberType(sym2)}\n" +
                s"match $rest")
            })
      }

      def AccessError(tree: Tree, sym: Symbol, ctx: Context, explanation: String): AbsTypeError =
        AccessError(tree, sym, ctx.enclClass.owner.thisType, ctx.enclClass.owner, explanation)

      def AccessError(tree: Tree, sym: Symbol, pre: Type, owner0: Symbol, explanation: String): AbsTypeError = {
        val errMsg = {
          val location = if (sym.isClassConstructor) s"in $owner0" else s"as a member of ${pre.widen.directObjectString}"
          val from = s" from ${owner0.fullLocationString}"

          underlyingSymbol(sym).fullLocationString + " cannot be accessed " + location + from + explanation
        }
        AccessTypeError(tree, errMsg)
      }

      def NoMethodInstanceError(fn: Tree, args: List[Tree], msg: String) =
        issueNormalTypeError(fn,
          "no type parameters for " +
          applyErrorMsg(fn, " exist so that it can be applied to arguments ", args map (_.tpe.widen), WildcardType) +
          "\n --- because ---\n" + msg)

      // TODO: no test case
      def NoConstructorInstanceError(tree: Tree, restpe: Type, pt: Type, msg: String): Unit = {
        issueNormalTypeError(tree,
          "constructor of type " + restpe +
          " cannot be uniquely instantiated to expected type " + pt +
          "\n --- because ---\n" + msg)
        setError(tree)
      }

      def ConstrInstantiationError(tree: Tree, restpe: Type, pt: Type) = {
        issueNormalTypeError(tree,
          "constructor cannot be instantiated to expected type" + foundReqMsg(restpe, pt))
        setError(tree)
      }

      // side-effect on the tree, break the overloaded type cycle in infer
      private def setErrorOnLastTry(lastTry: Boolean, tree: Tree) = if (lastTry) setError(tree)

      def widenArgs(argtpes: List[Type], params0: List[Symbol], params1: List[Symbol]): List[Type] =
        argtpes.zipWithIndex map {
          case (nt@NamedType(name, tp), _) => // a named argument
            (tp, params0.find(_.name == name).map(_.tpe), params1.find(_.name == name).map(_.tpe)) match {
              case (ConstantType(_), Some(ConstantType(_)), _) => nt
              case (ConstantType(_), _, Some(ConstantType(_))) => nt
              case (ct: ConstantType, _, _) => NamedType(name, ct.widen)
              case _ => nt
            }
          case (ct: ConstantType, pos) =>
            (params0.lift(pos).map(_.tpe), params1.lift(pos).map(_.tpe)) match {
              case (Some(ConstantType(_)), _) => ct
              case (_, Some(ConstantType(_))) => ct
              case _ => ct.widen
            }
          case (tpe, _) => tpe
        }

      def NoBestMethodAlternativeError(tree: Tree, argtpes: List[Type], pt: Type, lastTry: Boolean) = {
        val alts = alternatives(tree)
        val widenedArgtpes = widenArgs(argtpes, alts.head.params, alts.tail.head.params)
        val proscription =
          if (tree.symbol.isConstructor) " cannot be invoked with "
          else " cannot be applied to "
        val junkNames = {
          val bads = argtpes.collect {
            case NamedType(name, _) if !alts.exists(cond(_) { case MethodType(params, _) => params.exists(_.name == name) }) => name.decoded
          }
          if (bads.isEmpty) "" else bads.mkString(" [which have no such parameter ", ",", "]")
        }

        issueNormalTypeError(tree,
          applyErrorMsg(tree, junkNames + proscription, widenedArgtpes, pt))
        // since inferMethodAlternative modifies the state of the tree
        // we have to set the type of tree to ErrorType only in the very last
        // fallback action that is done in the inference.
        // This avoids entering infinite loop in doTypeApply.
        setErrorOnLastTry(lastTry, tree)
      }

      // If erroneous, do not even try further attempts because they should all fail
      // even if this is not the last attempt (because of the SO lurking beyond the horizon)
      def AmbiguousMethodAlternativeError(tree: Tree, pre: Type, best: Symbol,
            firstCompeting: Symbol, argtpes: List[Type], pt: Type, lastTry: Boolean) =
        if (argtpes.exists(_.isErroneous) || pt.isErroneous) setError(tree) else {
          def paramsOrEmpty(f: Symbol) = if (f.isMethod) f.asMethod.tpe.params else Nil
          val widenedArgtpes = widenArgs(argtpes, paramsOrEmpty(best), paramsOrEmpty(firstCompeting))
          val msg0 = widenedArgtpes.mkString("argument types (", ",", if (pt == WildcardType) ")" else s") and expected result type $pt")
          issueAmbiguousTypeErrorUnlessErroneous(tree.pos, pre, best, firstCompeting, msg0)
          setErrorOnLastTry(lastTry, tree)
        }

      def NoBestExprAlternativeError(tree: Tree, pt: Type, lastTry: Boolean) = {
        issueNormalTypeError(tree, withAddendum(tree.pos)(typeErrorMsg(context, tree.symbol.tpe, pt)))
        setErrorOnLastTry(lastTry, tree)
      }

      def AmbiguousExprAlternativeError(tree: Tree, pre: Type, best: Symbol, firstCompeting: Symbol, pt: Type, lastTry: Boolean) = {
        issueAmbiguousTypeErrorUnlessErroneous(tree.pos, pre, best, firstCompeting, "expected type " + pt)
        setErrorOnLastTry(lastTry, tree)
      }

      // checkBounds
      def KindBoundErrors(tree: Tree, prefix: String, targs: List[Type],
                          tparams: List[Symbol], kindErrors: List[String]) = {
        issueNormalTypeError(tree,
          prefix + "kinds of the type arguments " + targs.mkString("(", ",", ")") +
          " do not conform to the expected kinds of the type parameters "+
          tparams.mkString("(", ",", ")") + tparams.head.locationString+ "." +
          kindErrors.toList.mkString("\n", ", ", ""))
      }

      private[scala] def NotWithinBoundsErrorMessage(prefix: String, targs: List[Type], tparams: List[Symbol], explaintypes: Boolean): String = {
        if (explaintypes) {
          val bounds = tparams.map(_.info.instantiateTypeParams(tparams, targs).bounds)
          foreach2(targs, bounds)((targ, bound) => explainTypes(bound.lo, targ))
          foreach2(targs, bounds)((targ, bound) => explainTypes(targ, bound.hi))
        }
        def bracketed(items: List[_]) = items.mkString("[", ",", "]")
        val bounds = tparams.headOption.map(h => s"${h.owner}'s type parameter bounds ${bracketed(tparams.map(_.defString))}").getOrElse("empty type parameter list")

        s"${prefix}type arguments ${bracketed(targs)} do not conform to ${bounds}"
      }

      def NotWithinBounds(tree: Tree, prefix: String, targs: List[Type],
                          tparams: List[Symbol], kindErrors: List[String]) =
        issueNormalTypeError(tree,
          NotWithinBoundsErrorMessage(prefix, targs, tparams, settings.explaintypes.value))

      //substExpr
      def PolymorphicExpressionInstantiationError(tree: Tree, undetparams: List[Symbol], pt: Type) =
        issueNormalTypeError(tree,
          "polymorphic expression cannot be instantiated to expected type" +
          foundReqMsg(GenPolyType(undetparams, skipImplicit(tree.tpe)), pt))

      //checkCheckable
      def TypePatternOrIsInstanceTestError(tree: Tree, tp: Type) =
        issueNormalTypeError(tree, "type "+tp+" cannot be used in a type pattern or isInstanceOf test")

      def PatternTypeIncompatibleWithPtError1(tree: Tree, pattp: Type, pt: Type) =
        issueNormalTypeError(tree, "pattern type is incompatible with expected type" + foundReqMsg(pattp, pt))

      def IncompatibleScrutineeTypeError(tree: Tree, pattp: Type, pt: Type) =
        issueNormalTypeError(tree, "scrutinee is incompatible with pattern type" + foundReqMsg(pattp, pt))

      def PatternTypeIncompatibleWithPtError2(pat: Tree, pt1: Type, pt: Type) =
        issueNormalTypeError(pat,
          "pattern type is incompatible with expected type"+ foundReqMsg(pat.tpe, pt) +
          typePatternAdvice(pat.tpe.typeSymbol, pt1.typeSymbol))

      def PolyAlternativeError(tree: Tree, argtypes: List[Type], sym: Symbol, err: PolyAlternativeErrorKind.ErrorType) = {
        import PolyAlternativeErrorKind._
        val msg =
          err match {
            case WrongNumber =>
              "wrong number of type parameters for " + treeSymTypeMsg(tree)
            case NoParams =>
              treeSymTypeMsg(tree) + " does not take type parameters"
            case ArgsDoNotConform =>
              "type arguments " + argtypes.mkString("[", ",", "]") +
              " conform to the bounds of none of the overloaded alternatives of\n "+sym+
              ": "+sym.info
            case x => throw new MatchError(x)
          }
        issueNormalTypeError(tree, msg)
        ()
      }
    }
  }

  trait NamerContextErrors {
    self: Namer =>

    object NamerErrorGen {

      implicit val contextNamerErrorGen: Context = context

      object SymValidateErrors extends Enumeration {
        val ImplicitConstr, ImplicitNotTermOrClass, ImplicitAtToplevel,
          OverrideClass, SealedNonClass, AbstractNonClass,
          OverrideConstr, AbstractOverride, AbstractOverrideOnTypeMember, LazyAndEarlyInit,
          ByNameParameter, AbstractVar = Value
      }

      import SymValidateErrors._
      import symtab.Flags

      def TypeSigError(tree: Tree, ex: TypeError) = {
        ex match {
          case CyclicReference(_, _, _) if tree.symbol.isTermMacro =>
            // say, we have a macro def `foo` and its macro impl `impl`
            // if impl: 1) omits return type, 2) has anything implicit in its body, 3) sees foo
            //
            // then implicit search will trigger an error
            // (note that this is not a compilation error, it's an artifact of implicit search algorithm)
            // normally, such "errors" are discarded by `isCyclicOrErroneous` in Implicits.scala
            // but in our case this won't work, because isCyclicOrErroneous catches CyclicReference exceptions
            // while our error will present itself as a "recursive method needs a return type"
            //
            // hence we (together with reportTypeError in TypeDiagnostics) make sure that this CyclicReference
            // evades all the handlers on its way and successfully reaches `isCyclicOrErroneous` in Implicits
            throw ex
          case CyclicReference(sym, info: TypeCompleter, trace) =>
            issueNormalTypeError(tree, typer.cyclicReferenceMessage(sym, info.tree, trace, tree.pos).getOrElse(ex.getMessage))
          case _ =>
            contextNamerErrorGen.issue(TypeErrorWithUnderlyingTree(tree, ex))
        }
      }

      def GetterDefinedTwiceError(getter: Symbol) =
        issueSymbolTypeError(getter, s"$getter is defined twice")

      def ValOrVarWithSetterSuffixError(tree: Tree) =
        issueNormalTypeError(tree, "Names of vals or vars may not end in `_=`")

      def PrivateThisCaseClassParameterError(tree: Tree) =
        issueNormalTypeError(tree, "private[this] not allowed for case class parameters")

      def BeanPropertyAnnotationLimitationError(tree: Tree) =
        issueNormalTypeError(tree, "implementation limitation: the BeanProperty annotation cannot be used in a type alias or renamed import")

      def BeanPropertyAnnotationFieldWithoutLetterError(tree: Tree) =
        issueNormalTypeError(tree, "`BeanProperty` annotation can be applied only to fields that start with a letter")

      def BeanPropertyAnnotationPrivateFieldError(tree: Tree) =
        issueNormalTypeError(tree, "`BeanProperty` annotation can be applied only to non-private fields")

      def DoubleDefError(currentSym: Symbol, prevSym: Symbol) = {
        val s1 = if (prevSym.isModule) "case class companion " else ""
        val s2 = if (prevSym.isSynthetic) "(compiler-generated) " + s1 else ""
        val s3 = if (prevSym.isCase) "case class " + prevSym.name else "" + prevSym
        val where = if (currentSym.isTopLevel != prevSym.isTopLevel) {
                      val inOrOut = if (prevSym.isTopLevel) "outside of" else "in"
                      s" $inOrOut package object ${prevSym.effectiveOwner.name}"
                    } else ""

        issueSymbolTypeError(currentSym, s"${prevSym.name} is already defined as $s2$s3$where")
      }

      def MissingParameterOrValTypeError(vparam: Tree) =
        issueNormalTypeError(vparam, "missing parameter type")

      def ParentSealedInheritanceError(parent: Tree, psym: Symbol) =
        NormalTypeError(parent, "illegal inheritance from sealed " + psym )

      def SymbolValidationError(sym: Symbol, errKind: SymValidateErrors.Value): Unit = {
        val msg = errKind match {
          case ImplicitConstr               => "`implicit` modifier not allowed for constructors"
          case ImplicitNotTermOrClass       => "`implicit` modifier can be used only for values, variables, methods and classes"
          case ImplicitAtToplevel           => "`implicit` modifier cannot be used for top-level objects"
          case OverrideClass                => "`override` modifier not allowed for classes"
          case SealedNonClass               => "`sealed` modifier can be used only for classes"
          case AbstractNonClass             => "`abstract` modifier can be used only for classes; it should be omitted for abstract members"
          case OverrideConstr               => "`override` modifier not allowed for constructors"
          case AbstractOverride             => "`abstract override` modifier only allowed for members of traits"
          case AbstractOverrideOnTypeMember => "`abstract override` modifier not allowed for type members"
          case LazyAndEarlyInit             => "`lazy` definitions may not be initialized early"
          case ByNameParameter              => "pass-by-name arguments not allowed for case class parameters"
          case AbstractVar                  => "only traits and abstract classes can have declared but undefined members" + abstractVarMessage(sym)
          case x                            => throw new MatchError(x)
        }
        issueSymbolTypeError(sym, msg)
      }


      def AbstractMemberWithModiferError(sym: Symbol, flag: Long) =
        issueSymbolTypeError(sym, "abstract member may not have " + Flags.flagsToString(flag) + " modifier")

      def IllegalModifierCombination(sym: Symbol, flag1: Long, flag2: Long) =
        issueSymbolTypeError(sym, "illegal combination of modifiers: %s and %s for: %s".format(
            Flags.flagsToString(flag1), Flags.flagsToString(flag2), sym))

      def IllegalDependentMethTpeError(sym: Symbol)(context: Context) = {
        val errorAddendum =
          ": parameter may only be referenced in a subsequent parameter section"
        issueSymbolTypeError(sym,  "illegal dependent method type" + errorAddendum)(context)
      }
    }

    def InferredImplicitError(tree: Tree, inferred: Type, cx: Context): Unit =
      InferredImplicitErrorImpl(tree, inferred, cx, isTyper = false)
  }

  trait ImplicitsContextErrors {
    self: ImplicitSearch =>

    import definitions._

    def AmbiguousImplicitError(info1: ImplicitInfo, tree1: Tree,
                               info2: ImplicitInfo, tree2: Tree,
                               pre1: String, pre2: String, trailer: String)
                               (isView: Boolean, pt: Type, tree: Tree)(implicit context0: Context) = {
      if (!info1.tpe.isErroneous && !info2.tpe.isErroneous) {
        def coreMsg =
           sm"""| $pre1 ${info1.sym.fullLocationString} of type ${info1.tpe}
                | $pre2 ${info2.sym.fullLocationString} of type ${info2.tpe}
                | $trailer"""
        def viewMsg = {
          val found :: req :: _ = pt.typeArgs: @unchecked
          def explanation = {
            val sym = found.typeSymbol
            // Explain some common situations a bit more clearly. Some other
            // failures which have nothing to do with implicit conversions
            // per se, but which manifest as implicit conversion conflicts
            // involving Any, are further explained from foundReqMsg.
            if (AnyRefTpe <:< req) (
              if (sym == AnyClass || sym == UnitClass) (
                 sm"""|Note: ${sym.name} is not implicitly converted to AnyRef.  You can safely
                      |pattern match `x: AnyRef` or cast `x.asInstanceOf[AnyRef]` to do so."""
              )
              else boxedClass get sym map (boxed =>
                 sm"""|Note: an implicit exists from ${sym.fullName} => ${boxed.fullName}, but
                      |methods inherited from Object are rendered ambiguous.  This is to avoid
                      |a blanket implicit which would convert any ${sym.fullName} to any AnyRef.
                      |You may wish to use a type ascription: `x: ${boxed.fullName}`."""
              ) getOrElse ""
            )
            else
               sm"""|Note that implicit conversions are not applicable because they are ambiguous:
                    |${coreMsg}are possible conversion functions from $found to $req"""
          }
          typeErrorMsg(context, found, req) + (
            if (explanation == "") "" else "\n" + explanation
          )
        }

        // Note that treeInfo.Applied always matches, it just returns Nil when no application was found...
        def treeTypeArgs(annotatedTree: Tree): List[String] = annotatedTree match {
          case Block(_, Function(_, treeInfo.Applied(_, targs, _))) => targs.map(_.toString) // eta expansion, see neg/t9527b.scala
          case Function(_, treeInfo.Applied(_, targs, _)) => targs.map(_.toString) // eta expansion, see neg/t9527b.scala
          case treeInfo.Applied(_, targs, _) => targs.map(_.toString)
          case _ => Nil
        }

        context0.issueAmbiguousError(AmbiguousImplicitTypeError(tree,
          (info1.sym, info2.sym) match {
            case (ImplicitAmbiguousMsg(msg), _) => msg.formatDefSiteMessage(treeTypeArgs(tree1))
            case (_, ImplicitAmbiguousMsg(msg)) => msg.formatDefSiteMessage(treeTypeArgs(tree2))
            case (_, _) if isView => viewMsg
            case (_, _) => s"ambiguous implicit values:\n${coreMsg}match expected type $pt"
          }
        ))
      }
    }

    def DivergingImplicitExpansionError(tree: Tree, pt: Type, sym: Symbol)(implicit context0: Context) =
      issueTypeError(DivergentImplicitTypeError(tree, pt, sym))
  }

  object NamesDefaultsErrorsGen {
    import typer.infer.setError

    def NameClashError(sym: Symbol, arg: Tree)(implicit context: Context) = {
      setError(arg) // to distinguish it from ambiguous reference error

      def errMsg =
        "%s definition needs %s because '%s' is used as a named argument in its body.".format(
          "variable",   // "method"
          "type",       // "result type"
          sym.name)
      issueSymbolTypeError(sym, errMsg)
    }

    def UnknownParameterNameNamesDefaultError(arg: Tree, name: Name, warnVariableInScope: Boolean)(implicit context: Context) = {
      val suffix =
        if (warnVariableInScope)
          s"\nNote that assignments in argument position are no longer allowed since Scala 2.13.\nTo express the assignment expression, wrap it in brackets, e.g., `{ $name = ... }`."
        else ""
      issueNormalTypeError(arg, s"unknown parameter name: $name$suffix")
      setError(arg)
    }

    def DoubleParamNamesDefaultError(arg: Tree, name: Name, pos: Int, otherName: Option[Name])(implicit context: Context) = {
      val annex = otherName match {
        case Some(oName) => "\nNote that '"+ oName +"' is not a parameter name of the invoked method."
        case None => ""
      }
      issueNormalTypeError(arg, "parameter '"+ name +"' is already specified at parameter position "+ pos + annex)
      setError(arg)
    }

    def PositionalAfterNamedNamesDefaultError(arg: Tree)(implicit context: Context) = {
      issueNormalTypeError(arg, "positional after named argument.")
      setError(arg)
    }
  }
}
