/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

/**
 *  @author  Hubert Plociniczak
 *  @version 1.0
 */

import scala.collection.{ mutable, immutable }
import scala.tools.util.StringOps.{ countElementsAsString, countAsString }

trait ErrorTrees {
  self: Analyzer =>

  import global._

  trait ErrorTree extends AbsErrorTree {
    def emit(context: Context): Unit
    def emit(): Unit = emit(typer.context.asInstanceOf[Context])
    protected def errorSubtrees = Nil
    override def containsError() = true
    def exception: TypeError = null // Once we get rid of all thrown type errors (apart from cyclic), remove
    var reported = false
    override def tpe = ErrorType

    // Debugging option
    if (settings.errortrees.value)
      println("[ErrorTree instance] " + this.getClass)
  }

  trait ErrorPosAndMsg {
    def errMsg: String
    def errPos: Position
    def shouldEmit: Boolean
    def emit(context: Context): Unit
  }
  protected trait ContextError extends ErrorPosAndMsg { }

  trait ErrorTreeWithContext extends ErrorTree with ContextError {
    def shouldEmit = true
    def emit(context: Context) = if (shouldEmit) context.error(errPos, errMsg)
  }

  /** Traverses a tree, collecting subtrees for which the first argument is
   *  defined.  A given tree's children are traversed only if the keepTraversing
   *  predicate returns true for that tree.
   */
  def pruningCollect[T](pf: PartialFunction[Tree, T])(keepTraversing: Tree => Boolean): Tree => List[T] = {
    class Collector extends Traverser {
      val trees = mutable.ListBuffer[T]()
      override def traverse(t: Tree) {
        if (pf.isDefinedAt(t))
          trees += pf(t)
        if (keepTraversing(t))
          super.traverse(t)
      }
    }
    tree => {
      val c = new Collector
      c traverse tree
      c.trees.toList
    }
  }

  def errorTreesFinder(tree: Tree): List[ErrorTree] =
    pruningCollect({ case e: ErrorTree if !e.reported => e })(!_.isInstanceOf[ErrorTree])(tree)

  def quickErrorTreeFinder(tree: Tree): ErrorTree = tree find (_.isInstanceOf[ErrorTree]) match {
    case Some(x: ErrorTree) => x
    case _                  => NullErrorTree
  }

  protected abstract class TreeForwarder(forwardTo: Tree) extends Tree {
    override def pos       = forwardTo.pos
    override def hasSymbol = forwardTo.hasSymbol
    override def symbol    = forwardTo.symbol
    override def symbol_=(x: Symbol) = forwardTo.symbol = x
  }
  abstract class PositionedErrorTree(tree: Tree) extends ErrorTree with ErrorPosAndMsg {
    def errPos = tree.pos
    def shouldEmit = !tree.isErroneous
    def emit(context: Context) = if (shouldEmit) context.error(errPos, errMsg)
  }
  abstract class ContextErrorTree(tree: Tree) extends PositionedErrorTree(tree) with ContextError { }
  abstract class ErrorTreeForwarder(tree: Tree) extends TreeForwarder(tree) with ErrorTree with ErrorPosAndMsg {
    def errPos = tree.pos
    def shouldEmit = !tree.isErroneous
    def emit(context: Context) = if (shouldEmit) context.error(errPos, errMsg)
  }
  abstract class ContextErrorTreeForwarder(tree: Tree) extends ErrorTreeForwarder(tree) with ContextError { }

  // create trees for specific error trees

  trait TyperErrorTrees {
    self: Typer =>

    trait TypeErrorTrait extends ErrorTree with ErrorPosAndMsg {
      def errMsg = "type error"     // not used
      def emit(context: Context) {
        reportTypeError(context, errPos, exception)
      }
    }
    abstract class TypeErrorTreeForwarder(tree: Tree) extends ErrorTreeForwarder(tree) with TypeErrorTrait {
      override def emit(context: Context) {
        super[TypeErrorTrait].emit(context)
      }
    }

    import infer.setError

    case class UnstableTreeError(tree: Tree) extends ErrorTreeForwarder(tree) {
      private def addendum = {
        // !!! unused
        // val tpe = tree.symbol.tpe match {
        //   case PolyType(_, rtpe) => rtpe
        //   case t                 => t
        // }
        "\n Note that "+tree.symbol+" is not stable because its type, "+tree.tpe+", is volatile."
      }
      def errMsg = (
        "stable identifier required, but "+tree+" found." + (
          if (isStableExceptVolatile(tree)) addendum else ""
        )
      )
    }

    case class NoImplicitFoundError(tree: Tree, param: Symbol) extends ContextErrorTreeForwarder(tree) {
      def errMsg = {
        val paramName = param.name
        val paramTp   = param.tpe
        paramTp.typeSymbol match {
            case ImplicitNotFoundMsg(msg) => msg.format(paramName, paramTp)
            case _ =>
              "could not find implicit value for "+
                 (if (paramName startsWith nme.EVIDENCE_PARAM_PREFIX) "evidence parameter of type "
                  else "parameter "+paramName+": ")+paramTp
        }
      }
    }

    case class TypeErrorTree(tree: Tree, pt: Type, override val exception: TypeError) extends TypeErrorTreeForwarder(tree) { }

    case class AdaptToMemberWithArgsError(tree: Tree, override val exception: TypeError) extends TypeErrorTreeForwarder(tree) { }

    case class WithFilterError(tree: Tree, override val exception: TypeError) extends TypeErrorTreeForwarder(tree) {
      override def emit(context: Context) {
        super.emit(context)
        setError(tree)
      }
    }

    case class ParentTypesError(templ: Template, override val exception: TypeError) extends TypeErrorTrait {
      def errPos = templ.pos
      def shouldEmit = !templ.isErroneous
      override def emit(context: Context) {
        templ.tpe = null
        super.emit(context)
      }
    }

    // additional parentTypes errors
    case class ConstrArgsInTraitParentTpeError(arg: Tree, parent: Symbol) extends ContextErrorTree(arg) {
      def errMsg = parent + " is a trait; does not take constructor arguments"
    }

    case class MissingTypeArgumentsParentTpeError(supertpt: Tree) extends ContextErrorTree(supertpt) {
      def errMsg = "missing type arguments"
    }

    case class SilentTypeError(tree: Tree, override val exception: TypeError) extends TypeErrorTrait {
      def shouldEmit = false    // !!! correct?
      def errPos = tree.pos
    }

    case class TypedApplyError(tree: Tree, override val exception: TypeError) extends TypeErrorTreeForwarder(tree) {
      override def pos = exception.pos
    }

    case class AssignmentTypedApplyError(tree: Tree) extends ContextErrorTreeForwarder(tree) {
      def errMsg = "reassignment to val"
    }

    // typedIdent
    case class AmbiguousIdentError(tree: Tree, name: Name, msg: String) extends ContextErrorTreeForwarder(tree) {
      def errMsg = "reference to " + name + " is ambiguous;\n" + msg
    }

    case class SymbolNotFound(tree: Tree, name: Name, owner: Symbol) extends ContextErrorTreeForwarder(tree) {
      def errMsg = "not found: "+decodeWithKind(name, owner)
    }

    // typedAppliedTypeTree
    case class AppliedTypeNoParametersError(tree: Tree, errTpe: Type) extends ErrorTreeForwarder(tree) {
      def errMsg = errTpe + " does not take type parameters"
    }

    case class AppliedTypeWrongNumberOfArgsError(tree: Tree, errMsg: String) extends ErrorTreeForwarder(tree) { }

    // packagedef
    case class RefTreeError(tree: Tree, name: Name) extends ErrorTree with RefTree {
      // Error was already reported
      def emit(context: Context) { }
    }

    // typedTypeDef
    case class LowerBoundError(tree: TypeDef, lowB: Type, highB: Type) extends ContextErrorTree(tree) {
      def errMsg = "lower bound "+lowB+" does not conform to upper bound "+highB
    }

    // check privates
    case class HiddenSymbolWithError(tree: Tree) extends ErrorTree {
      def emit(context: Context) { }
    }

    case class SymbolEscapesScopeError(tree: Tree, badSymbol: Symbol) extends ErrorTreeWithContext {
      private val treeTpe = tree.tpe
      def errPos = tree.pos
      def errMsg = modifierString + badSymbol + " escapes its defining scope as part of type "+treeTpe
      private def modifierString = if (badSymbol.isPrivate) "private " else ""
    }

    // typedDefDef
    case class StarParamNotLastError(param: Tree) extends ContextErrorTree(param) {
      def errMsg = "*-parameter must come last"
    }

    case class StarWithDefaultError(meth: Symbol) extends ErrorTreeWithContext {
      def errMsg = "a parameter section with a `*'-parameter is not allowed to have default arguments"
      def errPos = meth.pos
    }

    case class InvalidConstructorDefError(ddef: Tree) extends ContextErrorTree(ddef) {
      def errMsg = "constructor definition not allowed here"
    }

    case class DeprecatedParamNameError(param: Symbol, name: Name) extends ErrorTreeWithContext {
      def errMsg = "deprecated parameter name "+ name +" has to be distinct from any other parameter name (deprecated or not)."
      def errPos = param.pos
    }

    // computeParamAliases
    case class SuperConstrReferenceError(tree: Tree) extends ContextErrorTree(tree) {
      def errMsg = "super constructor cannot be passed a self reference unless parameter is declared by-name"
    }

    case class SuperConstrArgsThisReferenceError(tree: Tree) extends ContextErrorTree(tree) {
      def errMsg = "super constructor arguments cannot reference unconstructed `this`"
    }

    // typedValDef
    case class VolatileValueError(vdef: Tree) extends ContextErrorTree(vdef) {
      def errMsg = "values cannot be volatile"
    }

    case class FinalVolatileVarError(vdef: Tree) extends ContextErrorTree(vdef) {
      def errMsg = "final vars cannot be volatile"
    }

    case class LocalVarUninitializedError(vdef: Tree) extends ContextErrorTree(vdef) {
      def errMsg = "local variables must be initialized"
    }

    //typedAssign
    case class AssignmentError(tree: Tree, varSym: Symbol) extends ContextErrorTreeForwarder(tree) {
      def errMsg =
        if (varSym != null && varSym.isValue) "reassignment to val"
        else "assignment to non variable"
    }

    case class UnexpectedTreeAssignmentConversionError(tree: Tree) extends ContextErrorTreeForwarder(tree) {
      def errMsg = "Unexpected tree during assignment conversion."
    }

    case class MultiDimensionalArrayError(tree: Tree) extends ContextErrorTree(tree) {
      def errMsg = "cannot create a generic multi-dimensional array of more than "+ definitions.MaxArrayDims+" dimensions"
    }

    //typedSuper
    case class MixinMissingParentClassNameError(tree: Tree, mix: Name, clazz: Symbol) extends ContextErrorTree(tree) {
      def errMsg = mix+" does not name a parent class of "+clazz
    }

    case class AmbiguousParentClassError(tree: Tree) extends ContextErrorTree(tree) {
      def errMsg = "ambiguous parent class qualifier"
    }

    //typedSelect
    case class NotAMemberErroneous(tree: Tree) extends ErrorTreeForwarder(tree) {
      def errMsg = ""
      override def emit(context: Context) { }
    }

    case class NotAMemberError(sel: Tree, qual: Tree, name: Name) extends ContextErrorTreeForwarder(sel) {
      def errMsg = {
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
            if (linePrecedes(qual, sel))
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
    }

    //typedNew
    case class IsAbstractError(tree: Tree, sym: Symbol) extends ContextErrorTree(tree) {
      def errMsg = sym + " is abstract; cannot be instantiated"
    }

    case class DoesNotConformToSelfTypeError(tree: Tree, sym: Symbol, tpe0: Type) extends ContextErrorTree(tree) {
      def errMsg = sym + " cannot be instantiated because it does not conform to its self-type " + tpe0
    }

    //typedEta
    case class UnderscoreEtaError(tree: Tree) extends PositionedErrorTree(tree) {
      def errMsg = "_ must follow method; cannot follow " + tree.tpe
    }

    //typedReturn
    case class ReturnOutsideOfDefError(tree: Tree) extends PositionedErrorTree(tree) {
      def errMsg = "return outside method definition"
    }

    case class ReturnWithoutTypeError(tree: Tree, owner: Symbol) extends PositionedErrorTree(tree) {
      def errMsg = owner + " has return statement; needs result type"
    }

    //typedBind
    case class VariableInPatternAlternativeError(tree: Tree) extends ContextErrorTree(tree) {
      def errMsg = "illegal variable in pattern alternative"
    }

    //typedCase
    case class StarPositionInPatternError(errPos: Position) extends ErrorTreeWithContext {
      def errMsg = "_* may only come last"
    }

    //typedFunction
    case class MaxFunctionArityError(fun: Tree) extends ErrorTreeForwarder(fun) {
      def errMsg = "implementation restricts functions to " + definitions.MaxFunctionArity + " parameters"
    }

    case class WrongNumberOfParametersError(tree: Tree, argpts: List[Type]) extends ErrorTreeForwarder(tree) {
      def errMsg = "wrong number of parameters; expected = " + argpts.length
    }

    case class MissingParameterTypeError(fun: Tree, vparam: ValDef, pt: Type) extends ContextErrorTree(vparam) {
      def anonMessage = (
          "\nThe argument types of an anonymous function must be fully known. (SLS 8.5)" +
          "\nExpected type was: " + pt.toLongString
        )

      private val suffix =
          if (!vparam.mods.isSynthetic) ""
          else " for expanded function" + (fun match {
            case Function(_, Match(_, _)) => anonMessage
            case _                        => " " + fun
          })

      def errMsg = "missing parameter type" + suffix
    }

    case class ConstructorsOrderError(tree: Tree) extends ContextErrorTree(tree) {
      def errMsg = "called constructor's definition must precede calling constructor's definition"
    }

    case class OnlyDeclarationsError(tree: Tree) extends ErrorTreeForwarder(tree) {
      def errMsg = "only declarations allowed here"
    }

    // typedAnnotation
    case class AnnotationNotAConstantError(tree: Tree) extends ContextErrorTree(tree) {
      def errMsg = "annotation argument needs to be a constant; found: " + tree
    }

    case class AnnotationArgNullError(tree: Tree) extends ContextErrorTree(tree) {
      def errMsg = "annotation argument cannot be null"
    }

    case class ArrayConstantsError(tree: Tree) extends ContextErrorTree(tree) {
      def errMsg = "Array constants have to be specified using the `Array(...)' factory method"
    }

    case class ArrayConstantsTypeMismatchError(tree: Tree, pt: Type) extends ContextErrorTree(tree) {
      def errMsg = "found array constant, expected argument of type " + pt
    }

    case class UnexpectedTreeAnnotation(tree: Tree) extends ContextErrorTree(tree) {
      def errMsg = "unexpected tree in annotation: "+ tree
    }

    case class AnnotationTypeMismatchError(tree: Tree, expected: Type, found: Type) extends ContextErrorTree(tree) {
      def errMsg = "expected annotation of type " + expected + ", found " + found
    }

    case class MultipleArgumentListForAnnotationError(tree: Tree) extends ContextErrorTree(tree) {
      def errMsg = "multiple argument lists on classfile annotation"
    }

    case class UnknownAnnotationNameError(tree: Tree, name: Name) extends ContextErrorTree(tree) {
      def errMsg = "unknown annotation argument name: " + name
    }

    case class DuplicateValueAnnotationError(tree: Tree, name: Name) extends ContextErrorTree(tree) {
      def errMsg = "duplicate value for annotation argument " + name
    }

    case class ClassfileAnnotationsAsNamedArgsError(tree: Tree) extends ContextErrorTree(tree) {
      def errMsg = "classfile annotation arguments have to be supplied as named arguments"
    }

    case class AnnotationMissingArgError(tree: Tree, annType: Type, name: Symbol) extends ContextErrorTree(tree) {
      def errMsg = "annotation " + annType.typeSymbol.fullName + " is missing argument " + name.name
    }

    case class NestedAnnotationError(tree: Tree, annType: Type) extends ContextErrorTree(tree) {
      def errMsg = "nested classfile annotations must be defined in java; found: "+ annType
    }

    case class UnexpectedTreeAnnotationError(tree: Tree, unexpected: Tree) extends ContextErrorTree(tree) {
      def errMsg = "unexpected tree after typing annotation: "+ unexpected
    }

    // TODO no test case
    //typedExistentialTypeTree
    case class AbstractionFromVolatileTypeError(vd: ValDef) extends ContextErrorTree(vd) {
      def errMsg = "illegal abstraction from value with volatile type "+vd.symbol.tpe
    }

    case class TypedApplyWrongNumberOfTpeParametersError(tree: Tree, fun: Tree) extends ErrorTreeForwarder(tree) {
      def errMsg = "wrong number of type parameters for "+treeSymTypeMsg(fun)
    }

    case class TypedApplyDoesNotTakeTpeParametersError(tree: Tree, fun: Tree) extends ErrorTreeForwarder(tree) {
      def errMsg = treeSymTypeMsg(fun)+" does not take type parameters."
    }

    // doTypeApply
      //tryNamesDefaults
    case class WrongNumberOfArgsError(tree: Tree, fun: Tree) extends ErrorTreeForwarder(tree) {
      def errMsg = "wrong number of arguments for "+ treeSymTypeMsg(fun)
    }

    case class TooManyArgsNamesDefaultsError(tree: Tree, fun: Tree) extends ErrorTreeForwarder(tree) {
      def errMsg = "too many arguments for "+treeSymTypeMsg(fun)
    }

    case class MultipleVarargError(tree: Tree) extends ErrorTreeForwarder(tree) {
      def errMsg = "when using named arguments, the vararg parameter has to be specified exactly once"
    }

    case class ModuleUsingCompanionClassDefaultArgsErrror(tree: Tree) extends ErrorTreeForwarder(tree) {
      def errMsg = "module extending its companion class cannot use default constructor arguments"
    }

    case class NotEnoughArgsError(tree: Tree, fun0: Tree, missing0: List[Symbol]) extends ErrorTreeForwarder(tree) {
      def errMsg = notEnoughArgumentsMsg(fun0, missing0)
      def notEnoughArgumentsMsg(fun: Tree, missing: List[Symbol]) = {
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
    }

      //doTypedApply - ErrorType
    case class ErroneousFunInTypeApplyError(fun: Tree, args: List[Tree]) extends TreeForwarder(fun) with ErrorTree {
      private lazy val errorCache = errorTreesFinder(fun) ++ (args flatMap errorTreesFinder)

      def emit(context: Context) {
        errorCache foreach (_ emit context)
      }
    }

      //doTypedApply - patternMode
    // TODO: missing test case
    case class TooManyArgsPatternError(fun: Tree) extends ContextErrorTreeForwarder(fun) {
      def errMsg = "too many arguments for unapply pattern, maximum = "+definitions.MaxTupleArity
    }

    case class WrongNumberArgsPatternError(tree: Tree, fun: Tree) extends ErrorTreeForwarder(tree) {
      def errMsg = "wrong number of arguments for "+treeSymTypeMsg(fun)
    }

    // Extends ErrorTreeWithPrettyPrinter to pass presentation/ping-pong test case
    case class ApplyWithoutArgsError(tree: Tree, fun: Tree) extends ErrorTreeForwarder(tree) with ErrorTreeWithPrettyPrinter {
      def errMsg = fun.tpe+" does not take parameters"
      override def toString = "" + tree
    }

    //checkClassType
    // When validating parents we sometimes should continue to
    // type the body of the template and sometimes not.
    // trait BlockingError allows us to distinguish it
    trait BlockingError

    case class TypeNotAStablePrefixError(pre: Type, errPos: Position) extends ErrorTreeWithContext with BlockingError {
      def errMsg = "type "+pre+" is not a stable prefix"
    }

    case class ClassTypeRequiredError(tree: Tree, found: AnyRef) extends ContextErrorTree(tree) with BlockingError {
      def errMsg = "class type required but "+found+" found"
    }

    // validateParentClasses
    case class ParentSuperSubclassError(errPos: Position, superclazz: Symbol,
                 parentSym: Symbol, mixin: Symbol)
      extends ErrorTreeWithContext {

      def errMsg = "illegal inheritance; super"+superclazz+
                   "\n is not a subclass of the super"+parentSym+
                   "\n of the mixin " + mixin
    }

    case class ParentNotATraitMixinError(errPos: Position, mixin: Symbol) extends ErrorTreeWithContext with BlockingError {
      def errMsg = mixin+" needs to be a trait to be mixed in"
    }

    case class ParentFinalInheritanceError(errPos: Position, mixin: Symbol) extends ErrorTreeWithContext with BlockingError {
      def errMsg = "illegal inheritance from final "+mixin
    }

    case class ParentSealedInheritanceError(errPos: Position, mixin: Symbol) extends ErrorTreeWithContext with BlockingError {
      def errMsg = "illegal inheritance from sealed "+mixin
    }

    case class ParentSelfTypeConformanceError(errPos: Position, selfType: Type, parent: Tree) extends ErrorTreeWithContext {
      def errMsg = (
        "illegal inheritance;\n self-type "+selfType+" does not conform to "+
        parent +"'s selftype "+parent.tpe.typeOfThis
      )
    }

    case class ParentInheritedTwiceError(errPos: Position, parent: Symbol) extends ErrorTreeWithContext with BlockingError {
      def errMsg = parent+" is inherited twice"
    }

    //adapt
    case class MissingArgsForMethodTpeError(tree: Tree, meth: Symbol) extends PositionedErrorTree(tree) {
      def errMsg = (
        "missing arguments for " + meth.fullLocationString + (
          if (meth.isConstructor) ""
          else ";\nfollow this method with `_' if you want to treat it as a partially applied function"
        )
      )
    }

    // This is really a workaround for a compiler bug
    case class Bug4425Error(tree: Tree) extends ContextErrorTreeForwarder(tree) {
      def errMsg = "erroneous or inaccessible type"
    }

    case class MissingTypeParametersError(tree: Tree) extends ErrorTreeForwarder(tree) {
      def errMsg = tree.symbol+" takes type parameters"

      // !!! Necessary?
      // override def emit(context: Context) {
      //   super.emit(context)
      //   setError(tree)
      // }
    }

    case class KindArityMismatchError(tree: Tree, pt: Type) extends ErrorTreeForwarder(tree) {
      def errMsg = (
        tree.tpe+" takes "+countElementsAsString(tree.tpe.typeParams.length, "type parameter")+
        ", expected: "+countAsString(pt.typeParams.length)
      )
    }
    //case class ParamsNotConvertible

    case class CaseClassConstructorError(tree: Tree) extends ErrorTreeForwarder(tree) {
      def errMsg = tree.symbol + " is not a case class constructor, nor does it have an unapply/unapplySeq method"
    }

    //TODO Needs test case
    case class ConstructorPrefixError(tree: Tree, restpe: Type) extends ContextErrorTree(tree) {
      def errMsg = restpe.prefix+" is not a legal prefix for a constructor"
    }

    // SelectFromTypeTree
    case class TypeSelectionFromVolatileTypeError(tree: Tree, qual: Tree) extends ContextErrorTree(tree) {
      def errMsg = "illegal type selection from volatile type "+qual.tpe
    }

    // packedType
    case class InferTypeWithVolatileTypeSelectionError(tree: Tree, pre: Type) extends ContextErrorTree(tree) {
      def errMsg = "Inferred type "+tree.tpe+" contains type selection from volatile type "+pre
    }

    case class AbstractExistentiallyOverParamerizedTpeError(tree: Tree, tp: Type) extends ContextErrorTree(tree) {
      def errMsg = "can't existentially abstract over parameterized type " + tp
    }

    //manifestTreee
    case class MissingManifestError(errPos: Position, full: Boolean, tp: Type) extends ErrorTreeWithContext {
      def errMsg = "cannot find "+(if (full) "" else "class ")+"manifest for element type "+tp
    }

    // TODO needs test case
    // cases where we do not necessairly return trees
    case class DependentMethodTpeConversionToFunctionError(errPos: Position, tp: Type) extends ErrorTreeWithContext {
      def errMsg = "method with dependent type "+tpe+" cannot be converted to function value"
      override def pos = errPos
    }

    //checkStarPatOK
    case class StarPatternWithVarargParametersError(errPos: Position) extends ErrorTreeWithContext {
      def errMsg = "star patterns must correspond with varargs parameters"
    }

    case class GetterDefinedTwiceError(getter: Symbol) extends ErrorTreeWithContext {
      def errMsg = getter+" is defined twice"
      def errPos = getter.pos
    }

    case class BeanPropertyAnnotationLimitationError(tree: Tree) extends ContextErrorTree(tree) {
      def errMsg = "implementation limitation: the BeanProperty annotation cannot be used in a type alias or renamed import"
    }

    // TODO missing test case
    case class FinitaryError(tparam: Symbol) extends ErrorTreeWithContext {
      def errMsg = "class graph is not finitary because type parameter "+tparam.name+" is expansively recursive"
      def errPos = tparam.pos
    }

    // TODO missing test case for a second case
    case class QualifyingClassError(tree: Tree, qual: Name) extends ContextErrorTree(tree) {
      def errMsg =
        if (qual.isEmpty) tree + " can be used only in a class, object, or template"
        else qual + " is not an enclosing class"
    }

    // def stabilize
    case class NotAValueError(tree: Tree, sym: Symbol) extends ErrorTreeForwarder(tree) {
      def errMsg = sym.kindString + " " + sym.fullName + " is not a value"
    }

    // checkNoDoubleDefs...
    case class DefDefinedTwiceError(sym0: Symbol, sym1: Symbol) extends ErrorTreeWithContext {
      def errMsg = sym1+" is defined twice"+{if(!settings.debug.value) "" else " in "+context.unit}
      def errPos = sym0.pos
      override def pos = sym0.pos
    }

    // cyclic errors
    case class CyclicAliasingOrSubtypingError(errPos: Position, sym0: Symbol) extends ErrorTreeWithContext {
      def errMsg = "cyclic aliasing or subtyping involving "+sym0
      override def pos = errPos
    }

    case class CyclicReferenceError(errPos: Position, lockedSym: Symbol) extends ErrorTreeWithContext {
      def errMsg = "illegal cyclic reference involving " + lockedSym
      override def pos = errPos
    }
  }

  trait InferencerErrorTrees {
    self: Inferencer =>

    private def applyErrorMsg(tree: Tree, msg: String, argtpes: List[Type], pt: Type) = {
      def asParams(xs: List[Any]) = xs.mkString("(", ", ", ")")

      def resType   = if (pt isWildcard) "" else " with expected result type " + pt
      def allTypes  = (alternatives(tree) flatMap (_.paramTypes)) ++ argtpes :+ pt
      def locals    = alternatives(tree) flatMap (_.typeParams)

      withDisambiguation(locals, allTypes: _*) {
        treeSymTypeMsg(tree) + msg + asParams(argtpes) + resType
      }
    }

    case class AccessError(tree: Tree, sym: Symbol, pre: Type, owner0: Symbol, explanation: String) extends ContextErrorTreeForwarder(tree) {
      def errMsg = {
        val realsym  = underlying(sym)
        val location = if (sym.isClassConstructor) owner0 else pre.widen

        realsym.fullLocationString + " cannot be accessed in " +
        location + explanation
      }
    }

    case class NoMethodInstanceError(fn: Tree, args: List[Tree], msg: String) extends ErrorTreeForwarder(fn) {
      def errMsg = (
        "no type parameters for " +
        applyErrorMsg(fn, " exist so that it can be applied to arguments ", args map (_.tpe.widen), WildcardType) +
        "\n --- because ---\n" + msg
      )
    }

    // TODO: no test case
    case class NoConstructorInstanceError(tree: Tree, restpe: Type, pt: Type, msg: String) extends ErrorTreeForwarder(tree) {
      def errMsg = (
        "constructor of type " + restpe +
        " cannot be uniquely instantiated to expected type " + pt +
        "\n --- because ---\n" + msg
      )
    }

    case class ConstrInstantiationError(tree: Tree, restpe: Type, pt: Type) extends ErrorTreeForwarder(tree) {
      def errMsg = "constructor cannot be instantiated to expected type" + foundReqMsg(restpe, pt)
    }

    case class NoBestMethodAlternativeError(tree: Tree, argtpes: List[Type], pt: Type) extends ErrorTreeForwarder(tree) {
      def errMsg = applyErrorMsg(tree, " cannot be applied to ", argtpes, pt)
    }

    case class AmbiguousMethodAlternativeError(tree: Tree, pre: Type, best: Symbol,
                 firstCompeting: Symbol, argtpes: List[Type], pt: Type)
      extends ErrorTreeForwarder(tree) {

      def errMsg = (
        "argument types " + argtpes.mkString("(", ",", ")") +
         (if (pt == WildcardType) "" else " and expected result type " + pt)
      )
      override def emit(context: Context) {
        context.ambiguousError(tree.pos, pre, best, firstCompeting, errMsg)
      }
    }

    case class NoBestExprAlternativeError(tree: Tree, pt: Type) extends ContextErrorTreeForwarder(tree) {
      def errMsg = withAddendum(tree.pos)(typeErrorMsg(tree.symbol.tpe, pt))
    }

    case class AmbiguousExprAlternativeError(tree: Tree, pre: Type, best: Symbol, firstCompeting: Symbol, pt: Type)
      extends ErrorTreeForwarder(tree) {

      def errMsg = "expected type " + pt
      override def emit(context: Context) {
        context.ambiguousError(tree.pos, pre, best, firstCompeting, errMsg)
      }
    }

    // checkBounds
    case class KindBoundErrors(errPos: Position, prefix: String, targs: List[Type],
                          tparams: List[Symbol], kindErrors: List[String])
      extends ErrorTreeWithContext {

      def errMsg = (
        prefix + "kinds of the type arguments " + targs.mkString("(", ",", ")") +
        " do not conform to the expected kinds of the type parameters "+
        tparams.mkString("(", ",", ")") + tparams.head.locationString+ "." +
        kindErrors.toList.mkString("\n", ", ", "")
      )
      override def pos = errPos
    }

    case class NotWithinBounds(pos0: Position, prefix: String, targs: List[Type],
                          tparams: List[Symbol], kindErrors: List[String])
      extends ErrorTree {

      val savedContext = getContext
      def emit(context: Context) {
        val validContext = if (context.unit == NoCompilationUnit) savedContext else context
        validContext.error(pos0,
                prefix + "type arguments " + targs.mkString("[", ",", "]") +
                " do not conform to " + tparams.head.owner + "'s type parameter bounds " +
                (tparams map (_.defString)).mkString("[", ",", "]"))
          if (settings.explaintypes.value) {
            val bounds = tparams map (tp => tp.info.instantiateTypeParams(tparams, targs).bounds)
            (targs, bounds).zipped foreach ((targ, bound) => explainTypes(bound.lo, targ))
            (targs, bounds).zipped foreach ((targ, bound) => explainTypes(targ, bound.hi))
            ()
          }
      }

      override def pos = pos0
    }

    //substExpr
    case class PolymorphicExpressionInstantiationError(tree: Tree, undetparams: List[Symbol], pt: Type) extends ContextErrorTreeForwarder(tree) {
      def errMsg = (
        "polymorphic expression cannot be instantiated to expected type" +
        foundReqMsg(polyType(undetparams, skipImplicit(tree.tpe)), pt)
      )
    }

    //checkCheckable
    case class TypePatternOrIsInstanceTestError(errPos: Position, tp: Type) extends ErrorTreeWithContext {
      def errMsg = "type "+tp+" cannot be used in a type pattern or isInstanceOf test"
      override def pos = errPos
    }

    case class IncompletePatternTypeError(errPos: Position, pattp: Type, pt: Type) extends ErrorTreeWithContext {
      def errMsg = "pattern type is incompatible with expected type" + foundReqMsg(pattp, pt)
      override def pos = errPos
    }

    case class IncompatibleScrutineeTypeError(errPos: Position, pattp: Type, pt: Type) extends ErrorTreeWithContext {
      def errMsg = "scrutinee is incompatible with pattern type" + foundReqMsg(pattp, pt)
      override def pos = errPos
    }

    case class PatternTypeIncompatibleWithPtError(pat: Tree, pt1: Type, pt: Type) extends ContextErrorTreeForwarder(pat) {
      def errMsg = {
        val sym   = pat.tpe.typeSymbol
        val clazz = sym.companionClass
        val addendum = (
          if (sym.isModuleClass && clazz.isCaseClass && (clazz isSubClass pt1.typeSymbol)) {
            // TODO: move these somewhere reusable.
            val typeString = clazz.typeParams match {
              case Nil  => "" + clazz.name
              case xs   => xs map (_ => "_") mkString (clazz.name + "[", ",", "]")
            }
            val caseString = (
              clazz.caseFieldAccessors
              map (_ => "_")    // could use the actual param names here
              mkString (clazz.name + "(", ",", ")")
            )
            (
              "\nNote: if you intended to match against the class, try `case _: " +
              typeString + "` or `case " + caseString + "`"
            )
          }
          else ""
        )
        "pattern type is incompatible with expected type"+foundReqMsg(pat.tpe, pt) + addendum
      }
    }

    case class PolyAlternativeError(tree: Tree, errMsg: String) extends ContextErrorTreeForwarder(tree) { }
  }

  trait NamerErrorTrees {
    self: Namer =>

      // Currently too general
    case class TypeSigError(tree: Tree, override val exception: TypeError) extends ErrorTree {
      def emit(context: Context) {
        typer.reportTypeError(context, tree.pos, exception)
      }
    }
  }

  // General errors
  case class PendingErrors(pending0: List[ErrorTree])
    extends ErrorTree {
    assert(pending0.nonEmpty, "pending exceptions cannot be empty")

    def emit(context: Context) {
      // Try to report each, here we dont' care
      // if any of those throws TypeError
      // this is handled in the actual application code
      pending0.foreach(_.emit(context))
    }

    override def pos = pending0.head.pos
    override def exception: TypeError = pending0.head.exception
  }

  case object NullErrorTree extends ErrorTree {
    def emit(context: Context) {}
  }

  case class SetErrorTree(tree: Tree) extends ErrorTree {
    def emit(context: Context) {
      typer.infer.setError(tree)
    }
  }

  //NamesDefaults errors, refactor to their own trait
  case class NameClashError(sym: Symbol, arg: Tree) extends ErrorTreeWithContext {
    def errMsg =
      "%s definition needs %s because '%s' is used as a named argument in its body.".format(
        "variable",   // "method"
        "type",       // "result type"
        sym.name)

    override def emit(context: Context) = {
      super.emit(context)
      // This is ugly hack to avoid reporting double errors
      // when dealing with AmbiguousReferences problem (error tree below) in names defaults.
      typer.infer.setError(arg)
    }

    def errPos = sym.pos
    override def pos = sym.pos
  }

  case class AmbiguousReferenceInNamesDefaultError(arg: Tree, name: Name) extends ErrorTreeForwarder(arg) {
    def errMsg = (
      "reference to "+ name +" is ambiguous; it is both, a parameter\n"+
      "name of the method and the name of a variable currently in scope."
    )
  }

  case class UnknownParameterNameNamesDefaultError(arg: Tree, name: Name) extends ErrorTreeForwarder(arg) {
    def errMsg = "unknown parameter name: " + name
  }

  case class DoubleParamNamesDefaultError(arg: Tree, name: Name) extends ErrorTreeForwarder(arg) {
    def errMsg = "parameter specified twice: "+ name
  }

  case class PositionalAfterNamedNamesDefaultError(arg: Tree) extends ErrorTreeForwarder(arg) {
    def errMsg = "positional after named argument."
  }
}
