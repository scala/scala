package scala.tools.nsc
package typechecker

trait ErrorTrees {
  self: Analyzer =>

  import global._

  trait ErrorTree extends AbsErrorTree {

    def emit(context: Context): Unit
    def emit(): Unit = emit(typer.context.asInstanceOf[Context])
    protected def initErrorCheck() {
      hasErrorTree = Some(true)
    }
    def exception: TypeError = null // Once we get rid of all thrown type errors (apart from cyclic), remove
    var reported = false
    override def tpe = ErrorType
    printName(this)
  }

  trait InteractiveErrorTree extends ErrorTree {
    def retrieveEmitted: Tree
  }

  trait ContextError {
    def errMsg: String
    def errPos: Position
    def emit(context: Context) = context.error(errPos, errMsg)
  }

  // Debugging option
  @inline private def printName(t: ErrorTree) {
    if (settings.errortrees.value)
      println("[ErrorTree instance] " + t.getClass)
  }

  object errorTreesFinder extends Traverser {
    import scala.collection.mutable
    var trees: mutable.ListBuffer[ErrorTree] = _
    override def traverse(t: Tree) {
      t match {
        case e: ErrorTree if !e.reported =>
          trees += e
        case e: ErrorTree =>
        case _ =>
          super.traverse(t)
      }
    }
    def apply(t: Tree) = {
      trees = new mutable.ListBuffer()
      traverse(t)
      trees
    }
  }

  object quickErrorTreeFinder extends Traverser {
    import scala.collection.mutable
    var found: Option[ErrorTree] = None
    override def traverse(t: Tree) {
      if (!found.isDefined)
        t match {
          case e: ErrorTree =>
            found = Some(e)
          case _ =>
            super.traverse(t)
        }
    }
    def apply(t: Tree) = {
      found = None
      traverse(t)
      found.get
    }
  }

  abstract class TreeForwarder(forwardTo: Tree) extends Tree {
    override def pos       = forwardTo.pos
    override def hasSymbol = forwardTo.hasSymbol
    override def symbol    = forwardTo.symbol
    override def symbol_=(x: Symbol) = forwardTo.symbol = x
  }

  // create trees for specific error trees

  trait TyperErrorTrees {
    self: Typer =>

    import infer.setError

    case class UnstableTreeError(tree: Tree)
      extends TreeForwarder(tree) with ErrorTree {

      def emit(context: Context) {
        val msg = "stable identifier required, but "+tree+" found."+
        (if (isStableExceptVolatile(tree)) {
          val tpe = tree.symbol.tpe match {
            case PolyType(_, rtpe) => rtpe
            case t => t
          }
          "\n Note that "+tree.symbol+" is not stable because its type, "+tree.tpe+", is volatile."
         } else "")

        if (!tree.isErroneous)
          context.error(tree.pos, msg)
      }
    }

    case class NoImplicitFoundError(fun: Tree, param: Symbol)
      extends TreeForwarder(fun) with ErrorTree with ContextError {

      def errMsg = {
        val paramName = param.name
        val paramTp = param.tpe
        paramTp.typeSymbol match {
            case ImplicitNotFoundMsg(msg) => msg.format(paramName, paramTp)
            case _ =>
              "could not find implicit value for "+
                 (if (paramName startsWith nme.EVIDENCE_PARAM_PREFIX) "evidence parameter of type "
                  else "parameter "+paramName+": ")+paramTp
        }
      }
      def errPos = fun.pos
    }

    case class TypeErrorTree(tree: Tree, pt: Type, override val exception: TypeError)
      extends TreeForwarder(tree) with ErrorTree {

      def emit(context: Context) {
        reportTypeError(context, tree.pos, exception)
      }
    }

    case class AdaptToMemberWithArgsError(tree: Tree, override val exception: TypeError)
      extends TreeForwarder(tree) with ErrorTree {

      def emit(context: Context) {
        reportTypeError(context, tree.pos, exception)
      }
    }

    case class WithFilterError(tree0: Tree, override val exception: TypeError)
      extends TreeForwarder(tree0) with ErrorTree {

      def emit(context: Context) {
        reportTypeError(context, tree0.pos, exception)
        setError(tree0)
      }
    }

    case class ParentTypesError(templ: Template, override val exception: TypeError)
      extends ErrorTree {

      def emit(context: Context) {
        templ.tpe = null
        reportTypeError(context, templ.pos, exception)
      }
    }

    // additional parentTypes errors

    case class ConstrArgsInTraitParentTpeError(arg: Tree, parent: Symbol)
      extends ErrorTree with ContextError {

      def errMsg = parent + " is a trait; does not take constructor arguments"
      def errPos = arg.pos
    }

    case class MissingTypeArgumentsParentTpeError(supertpt: Tree)
      extends ErrorTree with ContextError {

      def errMsg = "missing type arguments"
      def errPos = supertpt.pos
    }

    case class SilentTypeError(tree: Tree, override val exception: TypeError)
      extends ErrorTree {

      def emit(context: Context) {
        reportTypeError(context, tree.pos, exception)
      }
    }

    case class TypedApplyError(tree: Tree, override val exception: TypeError)
      extends TreeForwarder(tree) with ErrorTree {

      def emit(context: Context) {
        reportTypeError(context, tree.pos, exception)
      }

      override def pos = exception.pos
    }

    case class AssignmentTypedApplyError(tree: Tree)
      extends TreeForwarder(tree) with ErrorTree with ContextError {

      def errMsg = "reassignment to val"
      def errPos = tree.pos
    }


    // typedIdent
    case class AmbiguousIdentError(tree: Tree, name: Name, msg: String)
      extends TreeForwarder(tree) with ErrorTree with ContextError {

      def errMsg = "reference to " + name + " is ambiguous;\n" + msg
      def errPos = tree.pos
    }

    case class SymbolNotFound(tree: Tree, name: Name, owner: Symbol)
      extends TreeForwarder(tree) with ErrorTree with ContextError {

      def errMsg = "not found: "+decodeWithKind(name, owner)
      def errPos = tree.pos
    }

    // typedAppliedTypeTree
    case class AppliedTypeNoParametersError(tree: Tree, errTpe: Type)
      extends TreeForwarder(tree) with ErrorTree {

      def emit(context: Context) {
        if (!tree.isErroneous)
          context.error(tree.pos, errTpe + " does not take type parameters")
      }
    }

    case class AppliedTypeWrongNumberOfArgsError(tree: Tree, msg: String)
      extends TreeForwarder(tree) with ErrorTree {

      def emit(context: Context) {
        if (!tree.isErroneous)
          context.error(tree.pos, msg)
      }
    }

    // packagedef
    case class RefTreeError(tree: Tree, name: Name)
      extends ErrorTree with RefTree {

      def emit(context: Context) {
        // Error was already reported
      }
    }

    // typedTypeDef
    case class LowerBoundError(tree: TypeDef, lowB: Type, highB: Type)
      extends ErrorTree with ContextError {

      def errMsg = "lower bound "+lowB+" does not conform to upper bound "+highB
      def errPos = tree.pos
    }

    // check privates
    case class HiddenSymbolWithError(tree: Tree)
      extends ErrorTree {

      def emit(context: Context) {}
    }

    case class SymbolEscapesScopeError(tree: Tree, badSymbol: Symbol)
      extends ErrorTree with ContextError {

      val treeTpe = tree.tpe
      def errMsg = (if (badSymbol.isPrivate) "private " else "") + badSymbol +
                " escapes its defining scope as part of type "+treeTpe
      def errPos = tree.pos
    }

    // typedDefDef
    case class StarParamNotLastError(param: Tree)
      extends ErrorTree with ContextError {

      def errMsg = "*-parameter must come last"
      def errPos = param.pos
    }

    case class StarWithDefaultError(meth: Symbol)
      extends ErrorTree with ContextError {

      def errMsg = "a parameter section with a `*'-parameter is not allowed to have default arguments"
      def errPos = meth.pos
    }

    case class InvalidConstructorDefError(ddef: Tree)
      extends ErrorTree with ContextError {

      def errMsg = "constructor definition not allowed here"
      def errPos = ddef.pos
    }

    case class DeprecatedParamNameError(param: Symbol, name: Name)
      extends ErrorTree with ContextError {

      def errMsg = "deprecated parameter name "+ name +" has to be distinct from any other parameter name (deprecated or not)."
      def errPos = param.pos
    }

    // computeParamAliases
    case class SuperConstrReferenceError(tree: Tree)
      extends ErrorTree with ContextError {

      def errMsg = "super constructor cannot be passed a self reference unless parameter is declared by-name"
      def errPos = tree.pos
    }

    case class SuperConstrArgsThisReferenceError(tree: Tree)
      extends ErrorTree with ContextError {

      def errMsg = "super constructor arguments cannot reference unconstructed `this`"
      def errPos = tree.pos
    }

    // typedValDef
    case class VolatileValueError(vdef: Tree)
      extends ErrorTree with ContextError {

      def errMsg = "values cannot be volatile"
      def errPos = vdef.pos
    }

    case class FinalVolatileVarError(vdef: Tree)
      extends ErrorTree with ContextError {

      def errMsg = "final vars cannot be volatile"
      def errPos = vdef.pos
    }

    case class LocalVarUninitializedError(vdef: Tree)
      extends ErrorTree with ContextError {

      def errMsg = "local variables must be initialized"
      def errPos = vdef.pos
    }

    //typedAssign
    case class AssignmentError(tree: Tree, varSym: Symbol)
      extends TreeForwarder(tree) with ErrorTree with ContextError {

      def errMsg =
        if (varSym != null && varSym.isValue) "reassignment to val"
        else "assignment to non variable"
      def errPos = tree.pos
    }

    case class UnexpectedTreeAssignmentConversionError(tree: Tree)
      extends TreeForwarder(tree) with ErrorTree with ContextError {

      def errMsg = "Unexpected tree during assignment conversion."
      def errPos = tree.pos
    }

    case class MultiDimensionalArrayError(tree: Tree)
      extends ErrorTree with ContextError {

      def errMsg = "cannot create a generic multi-dimensional array of more than "+ definitions.MaxArrayDims+" dimensions"
      def errPos = tree.pos
    }

    //typedSuper
    case class MixinMissingParentClassNameError(tree: Tree, mix: Name, clazz: Symbol)
      extends ErrorTree with ContextError {

      def errMsg = mix+" does not name a parent class of "+clazz
      def errPos = tree.pos
    }

    case class AmbiguousParentClassError(tree: Tree)
      extends ErrorTree with ContextError {

      def errMsg = "ambiguous parent class qualifier"
      def errPos = tree.pos
    }

    //typedSelect
    case class NotAMemberErroneous(tree: Tree)
      extends TreeForwarder(tree) with ErrorTree {

      def emit(context: Context) { }
    }

    case class NotAMemberInteractive(originalTree: Tree)
      extends TreeForwarder(originalTree) with InteractiveErrorTree  {

      private[this] var newTree = originalTree

      def emit(context: Context) {
        def copyTree = {
            val tree1 = originalTree match {
              case Select(qual, name) => treeCopy.Select(originalTree, qual, name)
              case SelectFromTypeTree(qual, name) => treeCopy.SelectFromTypeTree(originalTree, qual, name)
            }
            tree1
          }
        newTree = copyTree
      }

      def retrieveEmitted = newTree
    }

    case class NotAMemberError(sel: Tree, qual: Tree, name: Name)
      extends TreeForwarder(sel) with ErrorTree with ContextError {

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

      def errPos = sel.pos
    }

    //typedNew
    case class IsAbstractError(tree: Tree, sym: Symbol)
      extends ErrorTree with ContextError {

      def errMsg = sym + " is abstract; cannot be instantiated"
      def errPos = tree.pos
    }

    case class DoesNotConformToSelfTypeError(tree: Tree, sym: Symbol, tpe0: Type)
      extends ErrorTree with ContextError {

      def errMsg = sym +
              " cannot be instantiated because it does not conform to its self-type "+
              tpe0
      def errPos = tree.pos
    }

    //typedEta
    case class UnderscoreEtaError(tree: Tree)
      extends ErrorTree {

      def emit(context: Context) {
        if (!tree.isErroneous)
          context.error(tree.pos, "_ must follow method; cannot follow " + tree.tpe)
      }
    }

    //typedReturn
    case class ReturnOutsideOfDefError(tree: Tree)
      extends ErrorTree {

      def emit(context: Context) {
        if (!tree.isErroneous)
          context.error(tree.pos, "return outside method definition")
      }
    }

    case class ReturnWithoutTypeError(tree: Tree, owner: Symbol)
      extends ErrorTree {

      def emit(context: Context) {
        if (!tree.isErroneous)
          context.error(tree.pos, owner + " has return statement; needs result type")
      }
    }

    //typedBind
    case class VariableInPatternAlternativeError(tree: Tree)
      extends ErrorTree with ContextError {

      def errMsg = "illegal variable in pattern alternative"
      def errPos = tree.pos
    }

    //typedCase
    case class StarPositionInPatternError(pos0: Position)
      extends ErrorTree with ContextError {

      def errMsg = "_* may only come last"
      def errPos = pos0
    }

    //typedFunction
    case class MaxFunctionArityError(fun: Tree)
      extends TreeForwarder(fun) with ErrorTree {

      def emit(context: Context) {
        if (!fun.isErroneous)
          context.error(fun.pos,"implementation restricts functions to " + definitions.MaxFunctionArity + " parameters")
      }
    }

    case class WrongNumberOfParametersError(tree: Tree, argpts: List[Type])
      extends TreeForwarder(tree) with ErrorTree {

      def emit(context: Context) {
        if (!tree.isErroneous)
          context.error(tree.pos,"wrong number of parameters; expected = " + argpts.length)
      }
    }

    case class MissingParameterTypeError(fun: Tree, vparam: ValDef, pt: Type)
      extends ErrorTree with ContextError {

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
      def errPos = vparam.pos
    }

    case class ConstructorsOrderError(tree: Tree)
      extends ErrorTree with ContextError {

      def errMsg = "called constructor's definition must precede calling constructor's definition"
      def errPos = tree.pos
    }

    case class OnlyDeclarationsError(tree: Tree)
      extends TreeForwarder(tree) with ErrorTree {

      def emit(context: Context) {
        if (!tree.isErroneous)
          context.error(tree.pos, "only declarations allowed here")
      }
    }

    // typedAnnotation
    case class AnnotationNotAConstantError(tree: Tree)
      extends ErrorTree with ContextError {

      def errMsg = "annotation argument needs to be a constant; found: " + tree
      def errPos = tree.pos
    }

    case class AnnotationArgNulError(tree: Tree)
      extends ErrorTree with ContextError {

      def errMsg = "annotation argument cannot be null"
      def errPos = tree.pos
    }

    case class ArrayConstantsError(tree: Tree)
      extends ErrorTree with ContextError {

      def errMsg = "Array constants have to be specified using the `Array(...)' factory method"
      def errPos = tree.pos
    }

    case class ArrayConstantsTypeMismatchError(tree: Tree, pt: Type)
      extends ErrorTree with ContextError {

      def errMsg = "found array constant, expected argument of type " + pt
      def errPos = tree.pos
    }

    case class UnexpectedTreeAnnotation(tree: Tree)
      extends ErrorTree with ContextError {

      def errMsg = "unexpected tree in annotation: "+ tree
      def errPos = tree.pos
    }

    case class AnnotationTypeMismatchError(tree: Tree, expected: Type, found: Type)
      extends ErrorTree with ContextError {

      def errMsg = "expected annotation of type "+ expected +", found "+ found
      def errPos = tree.pos
    }

    case class MultipleArgumentListForAnnotationError(tree: Tree)
      extends ErrorTree with ContextError {

      def errMsg = "multiple argument lists on classfile annotation"
      def errPos = tree.pos
    }

    case class UnknownAnnotationNameError(tree: Tree, name: Name)
      extends ErrorTree with ContextError {

      def errMsg = "unknown annotation argument name: " + name
      def errPos = tree.pos
    }

    case class DuplicateValueAnnotationError(tree: Tree, name: Name)
      extends ErrorTree with ContextError {

      def errMsg = "duplicate value for annotation argument " + name
      def errPos = tree.pos
    }

    case class ClassfileAnnotationsAsNamedArgsError(tree: Tree)
      extends ErrorTree with ContextError {

      def errMsg = "classfile annotation arguments have to be supplied as named arguments"
      def errPos = tree.pos
    }

    case class AnnotationMissingArgError(tree: Tree, annType: Type, name: Symbol)
      extends ErrorTree with ContextError {

      def errMsg = "annotation " + annType.typeSymbol.fullName + " is missing argument " + name.name
      def errPos = tree.pos
    }

    case class NestedAnnotationError(tree: Tree, annType: Type)
      extends ErrorTree with ContextError {

      def errMsg = "nested classfile annotations must be defined in java; found: "+ annType
      def errPos = tree.pos
    }

    case class UnexpectedTreeAnnotationError(tree: Tree, unexpected: Tree)
      extends ErrorTree with ContextError {

      def errMsg = "unexpected tree after typing annotation: "+ unexpected
      def errPos = tree.pos
    }

    // TODO no test case
    //typedExistentialTypeTree
    case class AbstractionFromVolatileTypeError(vd: ValDef)
      extends ErrorTree with ContextError {

      def errMsg = "illegal abstraction from value with volatile type "+vd.symbol.tpe
      def errPos = vd.pos
    }

    case class TypedApplyWrongNumberOfTpeParametersError(tree: Tree, fun: Tree)
      extends TreeForwarder(tree) with ErrorTree {

      def emit(context: Context) {
        if (!tree.isErroneous)
          context.error(tree.pos, "wrong number of type parameters for "+treeSymTypeMsg(fun))
      }
    }

    case class TypedApplyDoesNotTakeTpeParametersError(tree: Tree, fun: Tree)
      extends TreeForwarder(tree) with ErrorTree {

      def emit(context: Context) {
        if (!tree.isErroneous)
          context.error(tree.pos, treeSymTypeMsg(fun)+" does not take type parameters.")
      }
    }

    // doTypeApply
      //tryNamesDefaults
    case class WrongNumberOfArgsError(tree: Tree, fun: Tree)
      extends TreeForwarder(tree) with ErrorTree {

      def emit(context: Context) {
        if (!tree.isErroneous)
          context.error(tree.pos, "wrong number of arguments for "+ treeSymTypeMsg(fun))
      }
    }

    case class TooManyArgsNamesDefaultsError(tree: Tree, fun: Tree)
      extends TreeForwarder(tree) with ErrorTree {

      def emit(context: Context) {
        if (!tree.isErroneous)
          context.error(tree.pos, "too many arguments for "+treeSymTypeMsg(fun))
      }
    }

    case class MultipleVarargError(tree: Tree)
      extends TreeForwarder(tree) with ErrorTree {

      def emit(context: Context) {
        if (!tree.isErroneous)
          context.error(tree.pos, "when using named arguments, the vararg parameter "+
                                "has to be specified exactly once")
      }
    }

    case class ModuleUsingCompanionClassDefaultArgsErrror(tree: Tree)
      extends TreeForwarder(tree) with ErrorTree {

      def emit(context: Context) {
        if (!tree.isErroneous)
          context.error(tree.pos, "module extending its companion class cannot use default constructor arguments")
      }
    }

    case class NotEnoughArgsError(tree: Tree, fun0: Tree, missing0: List[Symbol])
      extends TreeForwarder(tree) with ErrorTree {
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

      def emit(context: Context) {
        if (!tree.isErroneous)
          context.error(tree.pos, notEnoughArgumentsMsg(fun0, missing0))
      }
    }

      //doTypedApply - ErrorType
    case class ErroneousFunInTypeApplyError(fun: Tree, args: List[Tree])
      extends TreeForwarder(fun) with ErrorTree {

      def emit(context: Context) {
        val all = errorTreesFinder(fun) ++ args.map(arg => errorTreesFinder(arg)).flatten
        all.foreach(_.emit(context))
      }
    }

      //doTypedApply - patternMode
    // TODO: missing test case
    case class TooManyArgsPatternError(fun: Tree)
      extends TreeForwarder(fun) with ErrorTree with ContextError {

      def errMsg = "too many arguments for unapply pattern, maximum = "+definitions.MaxTupleArity
      def errPos = fun.pos
    }

    case class WrongNumberArgsPatternError(tree: Tree, fun: Tree)
      extends TreeForwarder(tree) with ErrorTree {

      def emit(context: Context) {
        if (!tree.isErroneous)
          context.error(tree.pos, "wrong number of arguments for "+treeSymTypeMsg(fun))
      }
    }


    // Extends ErrorTreeWithPrettyPrinter to pass presentation/ping-pong test case
    case class ApplyWithoutArgsError(tree: Tree, fun: Tree)
      extends TreeForwarder(tree) with ErrorTreeWithPrettyPrinter with ErrorTree {

      def emit(context: Context) {
        if (!tree.isErroneous)
          context.error(tree.pos, fun.tpe+" does not take parameters")
      }

      override def toString() = tree.toString()
    }

    //checkClassType
    // When validating parents we sometimes should continue to
    // type the body of the template and sometimes not.
    // trait BlockingError allows us to distinguish it
    trait BlockingError

    case class TypeNotAStablePrefixError(pre: Type, pos0: Position)
      extends ErrorTree with BlockingError with ContextError {

      def errMsg = "type "+pre+" is not a stable prefix"
      def errPos = pos0
    }

    case class ClassTypeRequiredError(tree: Tree, found: AnyRef)
      extends ErrorTree with BlockingError with ContextError {

      def errMsg = "class type required but "+found+" found"
      def errPos = tree.pos
    }

    // validateParentClasses
    case class ParentSuperSubclassError(pos0: Position, superclazz: Symbol,
                 parentSym: Symbol, mixin: Symbol)
      extends ErrorTree with ContextError {

      def errMsg = "illegal inheritance; super"+superclazz+
                   "\n is not a subclass of the super"+parentSym+
                   "\n of the mixin " + mixin
      def errPos = pos0
    }

    case class ParentNotATraitMixinError(pos0: Position, mixin: Symbol)
      extends ErrorTree with BlockingError with ContextError {

      def errMsg = mixin+" needs to be a trait to be mixed in"
      def errPos = pos0
    }

    case class ParentFinalInheritanceError(pos0: Position, mixin: Symbol)
      extends ErrorTree with BlockingError with ContextError {

      def errMsg = "illegal inheritance from final "+mixin
      def errPos = pos0
    }

    case class ParentSealedInheritanceError(pos0: Position, mixin: Symbol)
      extends ErrorTree with BlockingError with ContextError {

      def errMsg = "illegal inheritance from sealed "+mixin
      def errPos = pos0
    }

    case class ParentSelfTypeConformanceError(
        pos0: Position, selfType: Type, parent: Tree)
      extends ErrorTree with ContextError {

      def errMsg = "illegal inheritance;\n self-type "+
                    selfType+" does not conform to "+parent +
                    "'s selftype "+parent.tpe.typeOfThis
      def errPos = pos0
    }

    case class ParentInheritedTwiceError(pos0: Position, parent: Symbol)
      extends ErrorTree with BlockingError with ContextError {

      def errMsg = parent+" is inherited twice"
      def errPos = pos0
    }

    //adapt
    case class MissingArgsForMethodTpeError(tree: Tree, meth: Symbol)
      extends ErrorTree {

      def emit(context: Context) {
        if (!tree.isErroneous)
          context.error(tree.pos, "missing arguments for "+meth+meth.locationString+
                    (if (meth.isConstructor) ""
                     else ";\nfollow this method with `_' if you want to treat it as a partially applied function"))
      }
    }

    // This is really a workaround for a compiler bug
    case class Bug4425Error(tree: Tree)
      extends TreeForwarder(tree) with ErrorTree with ContextError {

      def errMsg = "erroneous or inaccessible type"
      def errPos = tree.pos
    }

    case class MissingTypeParametersError(tree: Tree)
      extends TreeForwarder(tree) with ErrorTree {

      def emit(context: Context) {
        if (!tree.isErroneous)
          context.error(tree.pos, tree.symbol+" takes type parameters")
        setError(tree)
      }
    }

    case class KindArityMismatchError(tree: Tree, pt: Type)
      extends TreeForwarder(tree) with ErrorTree {

      def emit(context: Context) {
        import scala.tools.util.StringOps.{countElementsAsString, countAsString}
        if (!tree.isErroneous)
          context.error(tree.pos,
              tree.tpe+" takes "+countElementsAsString(tree.tpe.typeParams.length, "type parameter")+
              ", expected: "+countAsString(pt.typeParams.length))
      }
    }
    //case class ParamsNotConvertible

    case class CaseClassConstructorError(tree: Tree)
      extends TreeForwarder(tree) with ErrorTree {

      def emit(context: Context) {
        import scala.tools.util.StringOps.{countElementsAsString, countAsString}
        if (!tree.isErroneous)
          context.error(tree.pos,
              tree.symbol + " is not a case class constructor, nor does it have an unapply/unapplySeq method")
      }
    }

    //TODO Needs test case
    case class ConstructorPrefixError(tree: Tree, restpe: Type)
      extends ErrorTree with ContextError {

      def errMsg = restpe.prefix+" is not a legal prefix for a constructor"
      def errPos = tree.pos
    }

    // SelectFromTypeTree
    case class TypeSelectionFromVolatileTypeError(tree: Tree, qual: Tree)
      extends ErrorTree with ContextError {

      def errMsg = "illegal type selection from volatile type "+qual.tpe
      def errPos = tree.pos
    }

    // packedType
    case class InferTypeWithVolatileTypeSelectionError(tree: Tree, pre: Type)
      extends ErrorTree with ContextError {

      def errMsg = "Inferred type "+tree.tpe+" contains type selection from volatile type "+pre
      def errPos = tree.pos
    }

    case class AbstractExistentiallyOverParamerizedTpeError(tree: Tree, tp: Type)
      extends ErrorTree with ContextError {

      def errMsg = "can't existentially abstract over parameterized type " + tp
      def errPos = tree.pos
    }

    //manifestTreee
    case class MissingManifestError(pos0: Position, full: Boolean, tp: Type)
      extends ErrorTree with ContextError {

      def errMsg = "cannot find "+(if (full) "" else "class ")+"manifest for element type "+tp
      def errPos = pos0
    }


    // TODO needs test case
    // cases where we do not necessairly return trees
    case class DependentMethodTpeConversionToFunctionError(pos0: Position, tp: Type)
      extends ErrorTree with ContextError {

      def errMsg = "method with dependent type "+tpe+" cannot be converted to function value"
      def errPos = pos0

      override def pos = pos0
    }

    //checkStarPatOK
    case class StarPatternWithVarargParametersError(pos0: Position)
      extends ErrorTree with ContextError {

      def errMsg = "star patterns must correspond with varargs parameters"
      def errPos = pos0
    }

    case class GetterDefinedTwiceError(getter: Symbol)
      extends ErrorTree with ContextError {

      def errMsg = getter+" is defined twice"
      def errPos = getter.pos
    }

    case class BeanPropertyAnnotationLimitationError(tree: Tree)
      extends ErrorTree with ContextError {

      def errMsg = "implementation limitation: the BeanProperty annotation cannot be used in a type alias or renamed import"
      def errPos = tree.pos
    }

    // TODO missing test case
    case class FinitiaryError(tparam: Symbol)
      extends ErrorTree with ContextError {

      def errMsg = "class graph is not finitary because type parameter "+tparam.name+" is expansively recursive"
      def errPos = tparam.pos
    }

    // TODO missing test case for a second case
    case class QualifyingClassError(tree: Tree, qual: Name)
      extends ErrorTree with ContextError {

      def errMsg =
        if (qual.isEmpty) tree + " can be used only in a class, object, or template"
        else qual + " is not an enclosing class"
      def errPos = tree.pos
    }

    // def stabilize
    case class NotAValueError(tree: Tree, sym: Symbol)
      extends TreeForwarder(tree) with ErrorTree {

      def emit(context: Context) {
        if (!tree.isErroneous)
          context.error(tree.pos, sym.kindString + " " + sym.fullName + " is not a value")
      }
    }

    // checkNoDoubleDefs...
    case class DefDefinedTwiceError(sym0: Symbol, sym1: Symbol)
      extends ErrorTree with ContextError {

      def errMsg = sym1+" is defined twice"+{if(!settings.debug.value) "" else " in "+context.unit.toString}
      def errPos = sym0.pos
      override def pos = sym0.pos

    }

    // cyclic errors
    case class CyclicAliasingOrSubtypingError(pos0: Position, sym0: Symbol)
      extends ErrorTree with ContextError {

      def errMsg = "cyclic aliasing or subtyping involving "+sym0
      def errPos = pos0

      override def pos = pos0
    }

    case class CyclicReferenceError(pos0: Position, lockedSym: Symbol)
      extends ErrorTree with ContextError {

      def errMsg = "illegal cyclic reference involving " + lockedSym
      def errPos = pos0
      override def pos = pos0
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

    case class AccessError(tree: Tree, sym: Symbol, pre: Type,
          owner0: Symbol, explanation: String)
      extends TreeForwarder(tree) with ErrorTree with ContextError {

      def errMsg = {
        val realsym = underlying(sym)
        val location = if (sym.isClassConstructor) owner0 else pre.widen

        realsym.fullLocationString + " cannot be accessed in " +
        location + explanation
      }
      def errPos = tree.pos
    }

    case class NoMethodInstanceError(fn: Tree, args: List[Tree],
        msg: String)
      extends TreeForwarder(fn) with ErrorTree {

      def emit(context: Context) {
        if (!fn.isErroneous)
          context.error(fn.pos, "no type parameters for " +
            applyErrorMsg(fn, " exist so that it can be applied to arguments ", args map (_.tpe.widen), WildcardType) +
            "\n --- because ---\n" + msg)
      }
    }

    // TODO: no test case
    case class NoConstructorInstanceError(tree: Tree, restpe: Type,
        pt: Type, msg: String)
      extends TreeForwarder(tree) with ErrorTree {

      def emit(context: Context) {
        if (!tree.isErroneous)
          context.error(tree.pos, "constructor of type " + restpe +
                      " cannot be uniquely instantiated to expected type " + pt +
                      "\n --- because ---\n" + msg)
      }
    }

    case class ConstrInstantiationError(tree: Tree, restpe: Type,
        pt: Type)
      extends TreeForwarder(tree) with ErrorTree {

      def emit(context: Context) {
        if (!tree.isErroneous)
          context.error(tree.pos, "constructor cannot be instantiated to expected type" +
                  foundReqMsg(restpe, pt))
      }

    }

    case class NoBestMethodAlternativeError(tree: Tree, argtpes: List[Type],
        pt: Type)
      extends TreeForwarder(tree) with ErrorTree {

      def emit(context: Context) {
        if (!tree.isErroneous)
          context.error(tree.pos, applyErrorMsg(tree, " cannot be applied to ", argtpes, pt))
      }

    }

    case class AmbiguousMethodAlternativeError(tree: Tree, pre: Type, best: Symbol,
                 firstCompeting: Symbol, argtpes: List[Type], pt: Type)
      extends TreeForwarder(tree) with ErrorTree {

      def emit(context: Context) {
        context.ambiguousError(tree.pos, pre, best, firstCompeting,
                                     "argument types " + argtpes.mkString("(", ",", ")") +
                                     (if (pt == WildcardType) "" else " and expected result type " + pt))
      }
    }

    case class NoBestExprAlternativeError(tree: Tree, pt: Type)
      extends TreeForwarder(tree) with ErrorTree with ContextError {

      def errMsg = withAddendum(tree.pos)(typeErrorMsg(tree.symbol.tpe, pt))
      def errPos = tree.pos
    }

    case class AmbiguousExprAlternativeError(tree: Tree, pre: Type, best: Symbol,
                 firstCompeting: Symbol, pt: Type)
      extends TreeForwarder(tree) with ErrorTree {

      def emit(context: Context) {
        context.ambiguousError(tree.pos, pre, best, firstCompeting,
            "expected type " + pt)
      }
    }

    // checkBounds
    case class KindBoundErrors(pos0: Position, prefix: String, targs: List[Type],
                          tparams: List[Symbol], kindErrors: List[String])
      extends ErrorTree with ContextError {

      def errMsg =
          prefix + "kinds of the type arguments " + targs.mkString("(", ",", ")") +
          " do not conform to the expected kinds of the type parameters "+ tparams.mkString("(", ",", ")") + tparams.head.locationString+ "." +
          kindErrors.toList.mkString("\n", ", ", "")
      def errPos = pos0

      override def pos = pos0
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
    case class PolymorphicExpressionInstantiationError(tree: Tree, undetparams: List[Symbol],
      pt: Type)
      extends TreeForwarder(tree) with ErrorTree with ContextError {

      def errMsg =
        "polymorphic expression cannot be instantiated to expected type" +
        foundReqMsg(polyType(undetparams, skipImplicit(tree.tpe)), pt)
      def errPos = tree.pos
    }


    //checkCheckable
    case class TypePatternOrIsInstanceTestError(pos0: Position, tp: Type)
      extends ErrorTree with ContextError {

      def errMsg = "type "+tp+" cannot be used in a type pattern or isInstanceOf test"
      def errPos = pos0
      override def pos = pos0
    }

    case class IncompletePatternTypeError(pos0: Position, pattp: Type, pt: Type)
      extends ErrorTree with ContextError {

      def errMsg = "pattern type is incompatible with expected type" + foundReqMsg(pattp, pt)
      def errPos = pos0
      override def pos = pos0
    }

    case class IncompatibleScrutineeTypeError(pos0: Position, pattp: Type, pt: Type)
      extends ErrorTree with ContextError {

      def errMsg = "scrutinee is incompatible with pattern type" + foundReqMsg(pattp, pt)
      def errPos = pos0
      override def pos = pos0
    }

    case class PatternTypeIncompatibleWithPtError(pat: Tree, pt1: Type, pt: Type)
      extends TreeForwarder(pat) with ErrorTree with ContextError {

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
      def errPos = pat.pos
    }

    case class PolyAlternativeError(tree: Tree, msg: String)
      extends TreeForwarder(tree) with ErrorTree with ContextError {

      def errMsg = msg
      def errPos = tree.pos
    }
  }

  trait NamerErrorTrees {
    self: Namer =>

      // Currently too general
    case class TypeSigError(tree: Tree, override val exception: TypeError)
      extends ErrorTree {

      def emit(context: Context) {
        typer.reportTypeError(context, tree.pos, exception)
      }
    }
  }

  // General errors
  case class PendingErrors(pending0: List[ErrorTree])
    extends ErrorTree {
    assert(pending0.length != 0, "pending exceptions cannot be empty")


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
  case class NameClashError(sym: Symbol, arg: Tree)
    extends ErrorTree with ContextError {

    def errMsg =
      "%s definition needs %s because '%s' is used as a named argument in its body.".format(
        "variable",   // "method"
        "type",       // "result type"
        sym.name)
    def errPos = sym.pos

    override def emit(context: Context) = {
      super.emit(context)
      // This is ugly hack to avoid reporting double errors
      // when dealing with AmbiguousReferences problem (error tree below) in names defaults.
      typer.infer.setError(arg)
    }

    override def pos = sym.pos
  }

  case class AmbiguousReferenceInNamesDefaultError(arg: Tree, name: Name)
    extends TreeForwarder(arg) with ErrorTree {

    def emit(context: Context) {
      if (!arg.isErroneous) {
        context.error(
            arg.pos,
            "reference to "+ name +" is ambiguous; it is both, a parameter\n"+
            "name of the method and the name of a variable currently in scope.")
      }
    }
  }

  case class UnknwonParameterNameNamesDefaultError(arg: Tree, name: Name)
    extends TreeForwarder(arg) with ErrorTree {

    def emit(context: Context) {
      if (!arg.isErroneous)
        context.error(arg.pos, "unknown parameter name: " + name)
    }
  }

  case class DoubleParamNamesDefaultError(arg: Tree, name: Name)
    extends TreeForwarder(arg) with ErrorTree {

    def emit(context: Context) {
      if (!arg.isErroneous)
        context.error(arg.pos, "parameter specified twice: "+ name)
    }
  }

  case class PositionalAfterNamedNamesDefaultError(arg: Tree)
    extends TreeForwarder(arg) with ErrorTree {

    def emit(context: Context) {
      if (!arg.isErroneous)
        context.error(arg.pos, "positional after named argument.")
    }
  }
}