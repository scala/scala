/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

import scala.collection.{ mutable, immutable }
import scala.tools.util.StringOps.{ countElementsAsString, countAsString }
import symtab.Flags.{ PRIVATE, PROTECTED }

trait ContextErrors {
  self: Analyzer =>

  import global._

  object ErrorKinds extends Enumeration {
    type ErrorKind = Value
    val Normal, Access, Ambiguous, Divergent = Value
  }

  import ErrorKinds.ErrorKind

  trait AbsTypeError extends Throwable {
    def errPos: Position
    def errMsg: String
    def kind: ErrorKind
  }

  case class NormalTypeError(underlyingTree: Tree, errMsg: String, kind: ErrorKind = ErrorKinds.Normal)
    extends AbsTypeError {

    def errPos:Position = underlyingTree.pos
    override def toString() = "[Type error at:" + underlyingTree.pos + "] " + errMsg
  }

  case class SymbolTypeError(underlyingSym: Symbol, errMsg: String, kind: ErrorKind = ErrorKinds.Normal)
    extends AbsTypeError {

    def errPos = underlyingSym.pos
  }

  case class TypeErrorWrapper(ex: TypeError, kind: ErrorKind = ErrorKinds.Normal)
    extends AbsTypeError {
    def errMsg = ex.msg
    def errPos = ex.pos
  }

  case class TypeErrorWithUnderlyingTree(tree: Tree, ex: TypeError, kind: ErrorKind = ErrorKinds.Normal)
    extends AbsTypeError {
    def errMsg = ex.msg
    def errPos = tree.pos
  }

  case class AmbiguousTypeError(underlyingTree: Tree, errPos: Position, errMsg: String, kind: ErrorKind = ErrorKinds.Ambiguous) extends AbsTypeError

  case class PosAndMsgTypeError(errPos: Position, errMsg: String, kind: ErrorKind = ErrorKinds.Normal) extends AbsTypeError

  object ErrorUtils {
    def issueNormalTypeError(tree: Tree, msg: String)(implicit context: Context) {
      issueTypeError(NormalTypeError(tree, msg))
    }

    def issueSymbolTypeError(sym: Symbol, msg: String)(implicit context: Context) {
      issueTypeError(SymbolTypeError(sym, msg))
    }

    def issueDivergentImplicitsError(tree: Tree, msg: String)(implicit context: Context) {
      issueTypeError(NormalTypeError(tree, msg, ErrorKinds.Divergent))
    }

    def issueAmbiguousTypeError(pre: Type, sym1: Symbol, sym2: Symbol, err: AmbiguousTypeError)(implicit context: Context) {
      context.issueAmbiguousError(pre, sym1, sym2, err)
    }

    def issueTypeError(err: AbsTypeError)(implicit context: Context) { context.issue(err) }

    def typeErrorMsg(found: Type, req: Type, possiblyMissingArgs: Boolean) = {
      def missingArgsMsg = if (possiblyMissingArgs) "\n possible cause: missing arguments for method or constructor" else ""
      "type mismatch" + foundReqMsg(found, req) + missingArgsMsg
    }
  }

  import ErrorUtils._

  trait TyperContextErrors {
    self: Typer =>

    import infer.setError

    object TyperErrorGen {
      implicit val context0: Context = infer.getContext

      def UnstableTreeError(tree: Tree) = {
        def addendum = {
          "\n Note that "+tree.symbol+" is not stable because its type, "+tree.tpe+", is volatile."
        }
        issueNormalTypeError(tree,
          "stable identifier required, but "+tree+" found." + (
          if (isStableExceptVolatile(tree)) addendum else ""))
        setError(tree)
      }

      def NoImplicitFoundError(tree: Tree, param: Symbol) = {
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
        issueNormalTypeError(tree, errMsg)
      }

      def AdaptTypeError(tree: Tree, found: Type, req: Type) = {
        // If the expected type is a refinement type, and the found type is a refinement or an anon
        // class, we can greatly improve the error message by retyping the tree to recover the actual
        // members present, then display along with the expected members. This is done here because
        // this is the last point where we still have access to the original tree, rather than just
        // the found/req types.
        val foundType: Type = req.normalize match {
          case RefinedType(parents, decls) if !decls.isEmpty && found.typeSymbol.isAnonOrRefinementClass =>
            val retyped    = typed (tree.duplicate setType null)
            val foundDecls = retyped.tpe.decls filter (sym => !sym.isConstructor && !sym.isSynthetic)

            if (foundDecls.isEmpty || (found.typeSymbol eq NoSymbol)) found
            else {
              // The members arrive marked private, presumably because there was no
              // expected type and so they're considered members of an anon class.
              foundDecls foreach (_.makePublic)
              // TODO: if any of the found parents match up with required parents after normalization,
              // print the error so that they match. The major beneficiary there would be
              // java.lang.Object vs. AnyRef.
              refinedType(found.parents, found.typeSymbol.owner, foundDecls, tree.pos)
            }
          case _ =>
            found
        }
        assert(!found.isErroneous && !req.isErroneous, (found, req))

        issueNormalTypeError(tree, withAddendum(tree.pos)(typeErrorMsg(found, req, infer.isPossiblyMissingArgs(found, req))) )
        if (settings.explaintypes.value)
          explainTypes(found, req)
      }

      def WithFilterError(tree: Tree, ex: AbsTypeError) = {
        issueTypeError(ex)
        setError(tree)
      }

      def ParentTypesError(templ: Template, ex: TypeError) = {
         templ.tpe = null
         issueNormalTypeError(templ, ex.getMessage())
      }

      // additional parentTypes errors
      def ConstrArgsInTraitParentTpeError(arg: Tree, parent: Symbol) =
        issueNormalTypeError(arg, parent + " is a trait; does not take constructor arguments")

      def MissingTypeArgumentsParentTpeError(supertpt: Tree) =
        issueNormalTypeError(supertpt, "missing type arguments")

      // typedIdent
      def AmbiguousIdentError(tree: Tree, name: Name, msg: String) =
        NormalTypeError(tree, "reference to " + name + " is ambiguous;\n" + msg)

      def SymbolNotFoundError(tree: Tree, name: Name, owner: Symbol, startingIdentCx: Context) = {
        NormalTypeError(tree, "not found: "+decodeWithKind(name, owner))
      }

      // typedAppliedTypeTree
      def AppliedTypeNoParametersError(tree: Tree, errTpe: Type) = {
        issueNormalTypeError(tree, errTpe + " does not take type parameters")
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
        issueSymbolTypeError(meth, "a parameter section with a `*'-parameter is not allowed to have default arguments")

      def InvalidConstructorDefError(ddef: Tree) =
        issueNormalTypeError(ddef, "constructor definition not allowed here")

      def DeprecatedParamNameError(param: Symbol, name: Name) =
        issueSymbolTypeError(param, "deprecated parameter name "+ name +" has to be distinct from any other parameter name (deprecated or not).")

      // computeParamAliases
      def SuperConstrReferenceError(tree: Tree) =
        NormalTypeError(tree, "super constructor cannot be passed a self reference unless parameter is declared by-name")

      def SuperConstrArgsThisReferenceError(tree: Tree) =
        NormalTypeError(tree, "super constructor arguments cannot reference unconstructed `this`")

      // typedValDef
      def VolatileValueError(vdef: Tree) =
        issueNormalTypeError(vdef, "values cannot be volatile")

      def FinalVolatileVarError(vdef: Tree) =
        issueNormalTypeError(vdef, "final vars cannot be volatile")

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
        issueNormalTypeError(tree, mix+" does not name a parent class of "+clazz)

      def AmbiguousParentClassError(tree: Tree) =
        issueNormalTypeError(tree, "ambiguous parent class qualifier")

      //typedSelect
      def NotAMemberError(sel: Tree, qual: Tree, name: Name) = {
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
              else nameString + " is not a member of " + targetKindString + target.directObjectString + addendum
            )
        }
        issueNormalTypeError(sel, errMsg)
        // the error has to be set for the copied tree, otherwise
        // the error remains persistent acros multiple compilations
        // and causes problems
        //setError(sel)
      }

      //typedNew
      def IsAbstractError(tree: Tree, sym: Symbol) = {
        issueNormalTypeError(tree, sym + " is abstract; cannot be instantiated")
        setError(tree)
      }

      def DoesNotConformToSelfTypeError(tree: Tree, sym: Symbol, tpe0: Type) = {
        issueNormalTypeError(tree, sym + " cannot be instantiated because it does not conform to its self-type " + tpe0)
        setError(tree)
      }

      //typedEta
      def UnderscoreEtaError(tree: Tree) = {
        issueNormalTypeError(tree, "_ must follow method; cannot follow " + tree.tpe)
        setError(tree)
      }

      def MacroEtaError(tree: Tree) = {
        issueNormalTypeError(tree, "macros cannot be eta-expanded")
        setError(tree)
      }

      def MacroPartialApplicationError(tree: Tree) = {
        issueNormalTypeError(tree, "macros cannot be partially applied")
        setError(tree)
      }

      //typedReturn
      def ReturnOutsideOfDefError(tree: Tree) = {
        issueNormalTypeError(tree, "return outside method definition")
        setError(tree)
      }

      def ReturnWithoutTypeError(tree: Tree, owner: Symbol) = {
        issueNormalTypeError(tree, owner + " has return statement; needs result type")
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
      def MaxFunctionArityError(fun: Tree) = {
        issueNormalTypeError(fun, "implementation restricts functions to " + definitions.MaxFunctionArity + " parameters")
        setError(fun)
      }

      def WrongNumberOfParametersError(tree: Tree, argpts: List[Type]) = {
        issueNormalTypeError(tree, "wrong number of parameters; expected = " + argpts.length)
        setError(tree)
      }

      def MissingParameterTypeError(fun: Tree, vparam: ValDef, pt: Type) =
        if (vparam.mods.isSynthetic) fun match {
          case Function(_, Match(_, _)) => MissingParameterTypeAnonMatchError(vparam, pt)
          case _                        => issueNormalTypeError(vparam, "missing parameter type for expanded function " + fun)
        } else issueNormalTypeError(vparam, "missing parameter type")

      def MissingParameterTypeAnonMatchError(vparam: Tree, pt: Type) =
        issueNormalTypeError(vparam, "missing parameter type for expanded function\n"+
          "The argument types of an anonymous function must be fully known. (SLS 8.5)\n"+
          "Expected type was: " + pt.toLongString)

      def ConstructorsOrderError(tree: Tree) = {
        issueNormalTypeError(tree, "called constructor's definition must precede calling constructor's definition")
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
        NormalTypeError(tree, "Array constants have to be specified using the `Array(...)' factory method")

      def ArrayConstantsTypeMismatchError(tree: Tree, pt: Type) =
        NormalTypeError(tree, "found array constant, expected argument of type " + pt)

      def UnexpectedTreeAnnotation(tree: Tree) =
        NormalTypeError(tree, "unexpected tree in annotation: "+ tree)

      def AnnotationTypeMismatchError(tree: Tree, expected: Type, found: Type) =
        NormalTypeError(tree, "expected annotation of type " + expected + ", found " + found)

      def MultipleArgumentListForAnnotationError(tree: Tree) =
        NormalTypeError(tree, "multiple argument lists on classfile annotation")

      def UnknownAnnotationNameError(tree: Tree, name: Name) =
        NormalTypeError(tree, "unknown annotation argument name: " + name)

      def DuplicateValueAnnotationError(tree: Tree, name: Name) =
        NormalTypeError(tree, "duplicate value for annotation argument " + name)

      def ClassfileAnnotationsAsNamedArgsError(tree: Tree) =
        NormalTypeError(tree, "classfile annotation arguments have to be supplied as named arguments")

      def AnnotationMissingArgError(tree: Tree, annType: Type, sym: Symbol) =
        NormalTypeError(tree, "annotation " + annType.typeSymbol.fullName + " is missing argument " + sym.name)

      def NestedAnnotationError(tree: Tree, annType: Type) =
        NormalTypeError(tree, "nested classfile annotations must be defined in java; found: "+ annType)

      def UnexpectedTreeAnnotationError(tree: Tree, unexpected: Tree) =
        NormalTypeError(tree, "unexpected tree after typing annotation: "+ unexpected)

      //typedExistentialTypeTree
      def AbstractionFromVolatileTypeError(vd: ValDef) =
        issueNormalTypeError(vd, "illegal abstraction from value with volatile type "+vd.symbol.tpe)

      def TypedApplyWrongNumberOfTpeParametersError(tree: Tree, fun: Tree) = {
        issueNormalTypeError(tree, "wrong number of type parameters for "+treeSymTypeMsg(fun))
        setError(tree)
      }

      def TypedApplyDoesNotTakeTpeParametersError(tree: Tree, fun: Tree) = {
        issueNormalTypeError(tree, treeSymTypeMsg(fun)+" does not take type parameters.")
        setError(tree)
      }

      // doTypeApply
      //tryNamesDefaults
      def NamedAndDefaultArgumentsNotSupportedForMacros(tree: Tree, fun: Tree) =
        NormalTypeError(tree, "macros application do not support named and/or default arguments")

      def WrongNumberOfArgsError(tree: Tree, fun: Tree) =
        NormalTypeError(tree, "wrong number of arguments for "+ treeSymTypeMsg(fun))

      def TooManyArgsNamesDefaultsError(tree: Tree, fun: Tree) =
        NormalTypeError(tree, "too many arguments for "+treeSymTypeMsg(fun))

      // can it still happen? see test case neg/overloaded-unapply.scala
      def OverloadedUnapplyError(tree: Tree) =
        issueNormalTypeError(tree, "cannot resolve overloaded unapply")

      def UnapplyWithSingleArgError(tree: Tree) =
        issueNormalTypeError(tree, "an unapply method must accept a single argument.")

      def MultipleVarargError(tree: Tree) =
        NormalTypeError(tree, "when using named arguments, the vararg parameter has to be specified exactly once")

      def ModuleUsingCompanionClassDefaultArgsErrror(tree: Tree) =
        NormalTypeError(tree, "module extending its companion class cannot use default constructor arguments")

      def NotEnoughArgsError(tree: Tree, fun0: Tree, missing0: List[Symbol]) = {
        def notEnoughArgumentsMsg(fun: Tree, missing: List[Symbol]) = {
          val suffix = {
            if (missing.isEmpty) ""
            else {
              val keep = missing take 3 map (_.name)
              ".\nUnspecified value parameter%s %s".format(
                if (missing.tail.isEmpty) "" else "s",
                if ((missing drop 3).nonEmpty) (keep :+ "...").mkString(", ")
                else keep.mkString("", ", ", ".")
              )
            }
          }

          "not enough arguments for " + treeSymTypeMsg(fun) + suffix
        }
        NormalTypeError(tree, notEnoughArgumentsMsg(fun0, missing0))
      }

      //doTypedApply - patternMode
      def TooManyArgsPatternError(fun: Tree) =
        NormalTypeError(fun, "too many arguments for unapply pattern, maximum = "+definitions.MaxTupleArity)

      def WrongNumberArgsPatternError(tree: Tree, fun: Tree) =
        NormalTypeError(tree, "wrong number of arguments for "+treeSymTypeMsg(fun))

      def ApplyWithoutArgsError(tree: Tree, fun: Tree) =
        NormalTypeError(tree, fun.tpe+" does not take parameters")

      def DynamicVarArgUnsupported(tree: Tree, name: String) =
        issueNormalTypeError(tree, name+ " does not support passing a vararg parameter")

      //checkClassType
      def TypeNotAStablePrefixError(tpt: Tree, pre: Type) = {
        issueNormalTypeError(tpt, "type "+pre+" is not a stable prefix")
        setError(tpt)
      }

      def ClassTypeRequiredError(tree: Tree, found: AnyRef) = {
        issueNormalTypeError(tree, "class type required but "+found+" found")
        setError(tree)
      }

      // validateParentClasses
      def ParentSuperSubclassError(parent: Tree, superclazz: Symbol,
                 parentSym: Symbol, mixin: Symbol) =
        NormalTypeError(parent, "illegal inheritance; super"+superclazz+
                   "\n is not a subclass of the super"+parentSym+
                   "\n of the mixin " + mixin)

      def ParentNotATraitMixinError(parent: Tree, mixin: Symbol) =
        NormalTypeError(parent, mixin+" needs to be a trait to be mixed in")

      def ParentFinalInheritanceError(parent: Tree, mixin: Symbol) =
        NormalTypeError(parent, "illegal inheritance from final "+mixin)

      def ParentSealedInheritanceError(parent: Tree, psym: Symbol) =
        NormalTypeError(parent, "illegal inheritance from sealed " + psym )

      def ParentSelfTypeConformanceError(parent: Tree, selfType: Type) =
        NormalTypeError(parent,
          "illegal inheritance;\n self-type "+selfType+" does not conform to "+
          parent +"'s selftype "+parent.tpe.typeOfThis)

      def ParentInheritedTwiceError(parent: Tree, parentSym: Symbol) =
        NormalTypeError(parent, parentSym+" is inherited twice")

      //adapt
      def MissingArgsForMethodTpeError(tree: Tree, meth: Symbol) = {
        issueNormalTypeError(tree,
          "missing arguments for " + meth.fullLocationString + (
            if (meth.isConstructor) ""
            else ";\nfollow this method with `_' if you want to treat it as a partially applied function"
          ))
        setError(tree)
      }

      def MissingTypeParametersError(tree: Tree) = {
        issueNormalTypeError(tree, tree.symbol+" takes type parameters")
        setError(tree)
      }

      def KindArityMismatchError(tree: Tree, pt: Type) = {
        issueNormalTypeError(tree,
          tree.tpe+" takes "+countElementsAsString(tree.tpe.typeParams.length, "type parameter")+
          ", expected: "+countAsString(pt.typeParams.length))
        setError(tree)
      }

      def CaseClassConstructorError(tree: Tree) = {
        issueNormalTypeError(tree, tree.symbol + " is not a case class constructor, nor does it have an unapply/unapplySeq method")
        setError(tree)
      }

      def ConstructorPrefixError(tree: Tree, restpe: Type) = {
        issueNormalTypeError(tree, restpe.prefix+" is not a legal prefix for a constructor")
        setError(tree)
      }

      // SelectFromTypeTree
      def TypeSelectionFromVolatileTypeError(tree: Tree, qual: Tree) = {
        issueNormalTypeError(tree, "illegal type selection from volatile type "+qual.tpe)
        setError(tree)
      }

      // packedType
      def InferTypeWithVolatileTypeSelectionError(tree: Tree, pre: Type) =
        issueNormalTypeError(tree, "Inferred type "+tree.tpe+" contains type selection from volatile type "+pre)

      def AbstractExistentiallyOverParamerizedTpeError(tree: Tree, tp: Type) =
        issueNormalTypeError(tree, "can't existentially abstract over parameterized type " + tp)

      // resolveArrayTag
      def MissingArrayTagError(tree: Tree, tp: Type) = {
        issueNormalTypeError(tree, "cannot find array tag for element type "+tp)
        setError(tree)
      }

      // cases where we do not necessarily return trees
      def DependentMethodTpeConversionToFunctionError(tree: Tree, tp: Type) =
        issueNormalTypeError(tree, "method with dependent type "+tp+" cannot be converted to function value")

      //checkStarPatOK
      def StarPatternWithVarargParametersError(tree: Tree) =
        issueNormalTypeError(tree, "star patterns must correspond with varargs parameters")

      def FinitaryError(tparam: Symbol) =
        issueSymbolTypeError(tparam, "class graph is not finitary because type parameter "+tparam.name+" is expansively recursive")

      def QualifyingClassError(tree: Tree, qual: Name) = {
        issueNormalTypeError(tree,
          if (qual.isEmpty) tree + " can be used only in a class, object, or template"
          else qual + " is not an enclosing class")
        setError(tree)
      }

      // def stabilize
      def NotAValueError(tree: Tree, sym: Symbol) = {
        issueNormalTypeError(tree, sym.kindString + " " + sym.fullName + " is not a value")
        setError(tree)
      }

      def DefDefinedTwiceError(sym0: Symbol, sym1: Symbol) = {
        // Most of this hard work is associated with SI-4893.
        val isBug = sym0.isAbstractType && sym1.isAbstractType && (sym0.name startsWith "_$")
        val addendums = List(
          if (sym0.associatedFile eq sym1.associatedFile)
            Some("conflicting symbols both originated in file '%s'".format(sym0.associatedFile.canonicalPath))
          else if ((sym0.associatedFile ne null) && (sym1.associatedFile ne null))
            Some("conflicting symbols originated in files '%s' and '%s'".format(sym0.associatedFile.canonicalPath, sym1.associatedFile.canonicalPath))
          else None ,
          if (isBug) Some("Note: this may be due to a bug in the compiler involving wildcards in package objects") else None
        )
        val addendum = addendums.flatten match {
          case Nil    => ""
          case xs     => xs.mkString("\n  ", "\n  ", "")
        }

        issueSymbolTypeError(sym0, sym1+" is defined twice" + addendum)
      }

      // cyclic errors
     def CyclicAliasingOrSubtypingError(errPos: Position, sym0: Symbol) =
       issueTypeError(PosAndMsgTypeError(errPos, "cyclic aliasing or subtyping involving "+sym0))

     def CyclicReferenceError(errPos: Position, lockedSym: Symbol) =
       issueTypeError(PosAndMsgTypeError(errPos, "illegal cyclic reference involving " + lockedSym))
    }
  }

  trait InferencerContextErrors {
    self: Inferencer =>

    private def applyErrorMsg(tree: Tree, msg: String, argtpes: List[Type], pt: Type) = {
      def asParams(xs: List[Any]) = xs.mkString("(", ", ", ")")

      def resType   = if (pt.isWildcard) "" else " with expected result type " + pt
      def allTypes  = (alternatives(tree) flatMap (_.paramTypes)) ++ argtpes :+ pt
      def locals    = alternatives(tree) flatMap (_.typeParams)

      withDisambiguation(locals, allTypes: _*) {
        treeSymTypeMsg(tree) + msg + asParams(argtpes) + resType
      }
    }

    object InferErrorGen {

      implicit val context0 = getContext

      object PolyAlternativeErrorKind extends Enumeration {
        type ErrorType = Value
        val WrongNumber, NoParams, ArgsDoNotConform = Value
      }

      private def ambiguousErrorMsgPos(pos: Position, pre: Type, sym1: Symbol, sym2: Symbol, rest: String) =
        if (sym1.hasDefault && sym2.hasDefault && sym1.enclClass == sym2.enclClass) {
          val methodName = nme.defaultGetterToMethod(sym1.name)
          (sym1.enclClass.pos,
           "in "+ sym1.enclClass +", multiple overloaded alternatives of " + methodName +
                     " define default arguments")
        } else {
          (pos,
            ("ambiguous reference to overloaded definition,\n" +
             "both " + sym1 + sym1.locationString + " of type " + pre.memberType(sym1) +
             "\nand  " + sym2 + sym2.locationString + " of type " + pre.memberType(sym2) +
             "\nmatch " + rest)
          )
        }

      def AccessError(tree: Tree, sym: Symbol, pre: Type, owner0: Symbol, explanation: String) = {
        def errMsg = {
          val location = if (sym.isClassConstructor) owner0 else pre.widen.directObjectString

          underlyingSymbol(sym).fullLocationString + " cannot be accessed in " +
          location + explanation
        }
        NormalTypeError(tree, errMsg, ErrorKinds.Access)
      }

      def NoMethodInstanceError(fn: Tree, args: List[Tree], msg: String) =
        issueNormalTypeError(fn,
          "no type parameters for " +
          applyErrorMsg(fn, " exist so that it can be applied to arguments ", args map (_.tpe.widen), WildcardType) +
          "\n --- because ---\n" + msg)

      // TODO: no test case
      def NoConstructorInstanceError(tree: Tree, restpe: Type, pt: Type, msg: String) = {
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
      @inline
      private def setErrorOnLastTry(lastTry: Boolean, tree: Tree) = if (lastTry) setError(tree)
      
      def NoBestMethodAlternativeError(tree: Tree, argtpes: List[Type], pt: Type, lastTry: Boolean) = {
        issueNormalTypeError(tree,
          applyErrorMsg(tree, " cannot be applied to ", argtpes, pt))
        // since inferMethodAlternative modifies the state of the tree
        // we have to set the type of tree to ErrorType only in the very last
        // fallback action that is done in the inference.
        // This avoids entering infinite loop in doTypeApply.
        setErrorOnLastTry(lastTry, tree)
      }

      def AmbiguousMethodAlternativeError(tree: Tree, pre: Type, best: Symbol,
            firstCompeting: Symbol, argtpes: List[Type], pt: Type, lastTry: Boolean) = {
        
        if (!(argtpes exists (_.isErroneous)) && !pt.isErroneous) {
          val msg0 =
            "argument types " + argtpes.mkString("(", ",", ")") +
           (if (pt == WildcardType) "" else " and expected result type " + pt)
          val (pos, msg) = ambiguousErrorMsgPos(tree.pos, pre, best, firstCompeting, msg0)
          issueAmbiguousTypeError(pre, best, firstCompeting, AmbiguousTypeError(tree, pos, msg))
          setErrorOnLastTry(lastTry, tree)
        } else setError(tree) // do not even try further attempts because they should all fail
                              // even if this is not the last attempt (because of the SO's possibility on the horizon)
        
      }

      def NoBestExprAlternativeError(tree: Tree, pt: Type, lastTry: Boolean) = {
        issueNormalTypeError(tree, withAddendum(tree.pos)(typeErrorMsg(tree.symbol.tpe, pt, isPossiblyMissingArgs(tree.symbol.tpe, pt))))
        setErrorOnLastTry(lastTry, tree)
      }

      def AmbiguousExprAlternativeError(tree: Tree, pre: Type, best: Symbol, firstCompeting: Symbol, pt: Type, lastTry: Boolean) = {
        val (pos, msg) = ambiguousErrorMsgPos(tree.pos, pre, best, firstCompeting, "expected type " + pt)
        issueAmbiguousTypeError(pre, best, firstCompeting, AmbiguousTypeError(tree, pos, msg))
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

      def NotWithinBounds(tree: Tree, prefix: String, targs: List[Type],
                          tparams: List[Symbol], kindErrors: List[String]) = {
        if (settings.explaintypes.value) {
          val bounds = tparams map (tp => tp.info.instantiateTypeParams(tparams, targs).bounds)
          (targs, bounds).zipped foreach ((targ, bound) => explainTypes(bound.lo, targ))
          (targs, bounds).zipped foreach ((targ, bound) => explainTypes(targ, bound.hi))
          ()
        }

        issueNormalTypeError(tree,
                prefix + "type arguments " + targs.mkString("[", ",", "]") +
                " do not conform to " + tparams.head.owner + "'s type parameter bounds " +
                (tparams map (_.defString)).mkString("[", ",", "]"))
      }

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

      def PatternTypeIncompatibleWithPtError2(pat: Tree, pt1: Type, pt: Type) = {
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
        issueNormalTypeError(pat, errMsg)
      }

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
          }
        issueNormalTypeError(tree, msg)
        ()
      }
    }
  }

  trait NamerContextErrors {
    self: Namer =>

    object NamerErrorGen {

      implicit val context0 = context

      object SymValidateErrors extends Enumeration {
        val ImplicitConstr, ImplicitNotTermOrClass, ImplicitAtToplevel,
          OverrideClass, SealedNonClass, AbstractNonClass,
          OverrideConstr, AbstractOverride, LazyAndEarlyInit,
          ByNameParameter, AbstractVar = Value
      }

      object DuplicatesErrorKinds extends Enumeration {
        val RenamedTwice, AppearsTwice = Value
      }

      import SymValidateErrors._
      import DuplicatesErrorKinds._
      import symtab.Flags

      def TypeSigError(tree: Tree, ex: TypeError) = {
        ex match {
          case CyclicReference(_, _) if tree.symbol.isTermMacro =>
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
          case CyclicReference(sym, info: TypeCompleter) =>
            issueNormalTypeError(tree, typer.cyclicReferenceMessage(sym, info.tree) getOrElse ex.getMessage())
          case _ =>
            context0.issue(TypeErrorWithUnderlyingTree(tree, ex))
        }
      }

      def GetterDefinedTwiceError(getter: Symbol) =
        issueSymbolTypeError(getter, getter+" is defined twice")

      def ValOrValWithSetterSuffixError(tree: Tree) =
        issueNormalTypeError(tree, "Names of vals or vars may not end in `_='")

      def PrivateThisCaseClassParameterError(tree: Tree) =
        issueNormalTypeError(tree, "private[this] not allowed for case class parameters")

      def BeanPropertyAnnotationLimitationError(tree: Tree) =
        issueNormalTypeError(tree, "implementation limitation: the BeanProperty annotation cannot be used in a type alias or renamed import")

      def BeanPropertyAnnotationFieldWithoutLetterError(tree: Tree) =
        issueNormalTypeError(tree, "`BeanProperty' annotation can be applied only to fields that start with a letter")

      def BeanPropertyAnnotationPrivateFieldError(tree: Tree) =
        issueNormalTypeError(tree, "`BeanProperty' annotation can be applied only to non-private fields")

      def DoubleDefError(currentSym: Symbol, prevSym: Symbol) = {
        val s1 = if (prevSym.isModule) "case class companion " else ""
        val s2 = if (prevSym.isSynthetic) "(compiler-generated) " + s1 else ""
        val s3 = if (prevSym.isCase) "case class " + prevSym.name else "" + prevSym
        val where = if (currentSym.owner.isPackageClass != prevSym.owner.isPackageClass) {
                      val inOrOut = if (prevSym.owner.isPackageClass) "outside of" else "in"
                      " %s package object %s".format(inOrOut, ""+prevSym.effectiveOwner.name)
                    } else ""

        issueSymbolTypeError(currentSym, prevSym.name + " is already defined as " + s2 + s3 + where)
      }

      def MaxParametersCaseClassError(tree: Tree) =
        issueNormalTypeError(tree, "Implementation restriction: case classes cannot have more than " + definitions.MaxFunctionArity + " parameters.")

      def InheritsItselfError(tree: Tree) =
        issueNormalTypeError(tree, tree.tpe.typeSymbol+" inherits itself")

      def MissingParameterOrValTypeError(vparam: Tree) =
        issueNormalTypeError(vparam, "missing parameter type")

      def RootImportError(tree: Tree) =
        issueNormalTypeError(tree, "_root_ cannot be imported")

      def SymbolValidationError(sym: Symbol, errKind: SymValidateErrors.Value) {
        val msg = errKind match {
          case ImplicitConstr =>
            "`implicit' modifier not allowed for constructors"

          case ImplicitNotTermOrClass =>
            "`implicit' modifier can be used only for values, variables, methods and classes"

          case ImplicitAtToplevel =>
            "`implicit' modifier cannot be used for top-level objects"

          case OverrideClass =>
            "`override' modifier not allowed for classes"

          case SealedNonClass =>
            "`sealed' modifier can be used only for classes"

          case AbstractNonClass =>
            "`abstract' modifier can be used only for classes; it should be omitted for abstract members"

          case OverrideConstr =>
            "`override' modifier not allowed for constructors"

          case AbstractOverride =>
            "`abstract override' modifier only allowed for members of traits"

          case LazyAndEarlyInit =>
            "`lazy' definitions may not be initialized early"

          case ByNameParameter =>
            "pass-by-name arguments not allowed for case class parameters"

          case AbstractVar =>
            "only classes can have declared but undefined members" + abstractVarMessage(sym)

        }
        issueSymbolTypeError(sym, msg)
      }


      def AbstractMemberWithModiferError(sym: Symbol, flag: Int) =
        issueSymbolTypeError(sym, "abstract member may not have " + Flags.flagsToString(flag) + " modifier")

      def IllegalModifierCombination(sym: Symbol, flag1: Int, flag2: Int) =
        issueSymbolTypeError(sym, "illegal combination of modifiers: %s and %s for: %s".format(
            Flags.flagsToString(flag1), Flags.flagsToString(flag2), sym))

      def IllegalDependentMethTpeError(sym: Symbol)(context: Context) = {
        val errorAddendum =
          ": parameter appears in the type of another parameter in the same section or an earlier one"
        issueSymbolTypeError(sym,  "illegal dependent method type" + errorAddendum)(context)
      }

      def DuplicatesError(tree: Tree, name: Name, kind: DuplicatesErrorKinds.Value) = {
        val msg = kind match {
          case RenamedTwice =>
            "is renamed twice"
          case AppearsTwice =>
            "appears twice as a target of a renaming"
        }

        issueNormalTypeError(tree, name.decode + " " + msg)
      }
    }
  }

  trait ImplicitsContextErrors {
    self: ImplicitSearch =>

    import definitions._

    def AmbiguousImplicitError(info1: ImplicitInfo, info2: ImplicitInfo,
                               pre1: String, pre2: String, trailer: String)
                               (isView: Boolean, pt: Type, tree: Tree)(implicit context0: Context) = {
      if (!info1.tpe.isErroneous && !info2.tpe.isErroneous) {
        val coreMsg =
          pre1+" "+info1.sym.fullLocationString+" of type "+info1.tpe+"\n "+
          pre2+" "+info2.sym.fullLocationString+" of type "+info2.tpe+"\n "+
          trailer
        val errMsg =
          if (isView) {
            val found = pt.typeArgs(0)
            val req = pt.typeArgs(1)
            def defaultExplanation =
              "Note that implicit conversions are not applicable because they are ambiguous:\n "+
              coreMsg+"are possible conversion functions from "+ found+" to "+req

            def explanation = {
              val sym = found.typeSymbol
              // Explain some common situations a bit more clearly.
              if (AnyRefClass.tpe <:< req) {
                if (sym == AnyClass || sym == UnitClass) {
                  "Note: " + sym.name + " is not implicitly converted to AnyRef.  You can safely\n" +
                  "pattern match `x: AnyRef` or cast `x.asInstanceOf[AnyRef]` to do so."
                }
                else boxedClass get sym match {
                  case Some(boxed)  =>
                    "Note: an implicit exists from " + sym.fullName + " => " + boxed.fullName + ", but\n" +
                    "methods inherited from Object are rendered ambiguous.  This is to avoid\n" +
                    "a blanket implicit which would convert any " + sym.fullName + " to any AnyRef.\n" +
                    "You may wish to use a type ascription: `x: " + boxed.fullName + "`."
                  case _ =>
                    defaultExplanation
                }
              }
              else defaultExplanation
            }

            typeErrorMsg(found, req, infer.isPossiblyMissingArgs(found, req)) + "\n" + explanation
          } else {
            "ambiguous implicit values:\n "+coreMsg + "match expected type "+pt
          }
        context.issueAmbiguousError(AmbiguousTypeError(tree, tree.pos, errMsg))
      }
    }

    def DivergingImplicitExpansionError(tree: Tree, pt: Type, sym: Symbol)(implicit context0: Context) =
      issueDivergentImplicitsError(tree,
          "diverging implicit expansion for type "+pt+"\nstarting with "+
          sym.fullLocationString)
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

    def AmbiguousReferenceInNamesDefaultError(arg: Tree, name: Name)(implicit context: Context) = {
      if (!arg.isErroneous) { // check if name clash wasn't reported already
        issueNormalTypeError(arg,
          "reference to "+ name +" is ambiguous; it is both a method parameter "+
          "and a variable in scope.")
        setError(arg)
      } else arg
    }
    
    def WarnAfterNonSilentRecursiveInference(param: Symbol, arg: Tree)(implicit context: Context) = {
      val note = "type-checking the invocation of "+ param.owner +" checks if the named argument expression '"+ param.name + " = ...' is a valid assignment\n"+
                 "in the current scope. The resulting type inference error (see above) can be fixed by providing an explicit type in the local definition for "+ param.name +"."
      context.warning(arg.pos, note)
    }

    def UnknownParameterNameNamesDefaultError(arg: Tree, name: Name)(implicit context: Context) = {
      issueNormalTypeError(arg, "unknown parameter name: " + name)
      setError(arg)
    }

    def DoubleParamNamesDefaultError(arg: Tree, name: Name, pos: Int, otherName: Option[Name])(implicit context: Context) = {
      val annex = otherName match {
        case Some(oName) => "\nNote that that '"+ oName +"' is not a parameter name of the invoked method."
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
