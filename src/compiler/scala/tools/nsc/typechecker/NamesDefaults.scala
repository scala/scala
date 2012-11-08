/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

import symtab.Flags._
import scala.collection.mutable
import scala.reflect.ClassTag

/**
 *  @author Lukas Rytz
 *  @version 1.0
 */
trait NamesDefaults { self: Analyzer =>

  import global._
  import definitions._
  import NamesDefaultsErrorsGen._

  // Default getters of constructors are added to the companion object in the
  // typeCompleter of the constructor (methodSig). To compute the signature,
  // we need the ClassDef. To create and enter the symbols into the companion
  // object, we need the templateNamer of that module class. These two are stored
  // as an attachment in the companion module symbol
  class ConstructorDefaultsAttachment(val classWithDefault: ClassDef, var companionModuleClassNamer: Namer)

  // To attach the default getters of local (term-owned) methods to the method symbol.
  // Used in Namer.enterExistingSym: it needs to re-enter the method symbol and also
  // default getters, which could not be found otherwise.
  class DefaultsOfLocalMethodAttachment(val defaultGetters: mutable.Set[Symbol]) {
    def this(default: Symbol) = this(mutable.Set(default))
  }

  case class NamedApplyInfo(
    qual:       Option[Tree],
    targs:      List[Tree],
    vargss:     List[List[Tree]],
    blockTyper: Typer
  ) { }

  val noApplyInfo = NamedApplyInfo(None, Nil, Nil, null)

  def nameOf(arg: Tree) = arg match {
    case AssignOrNamedArg(Ident(name), rhs) => Some(name)
    case _ => None
  }
  def isNamed(arg: Tree) = nameOf(arg).isDefined

  /** @param pos maps indices from old to new */
  def reorderArgs[T: ClassTag](args: List[T], pos: Int => Int): List[T] = {
    val res = new Array[T](args.length)
    foreachWithIndex(args)((arg, index) => res(pos(index)) = arg)
    res.toList
  }

  /** @param pos maps indices from new to old (!) */
  def reorderArgsInv[T: ClassTag](args: List[T], pos: Int => Int): List[T] = {
    val argsArray = args.toArray
    (argsArray.indices map (i => argsArray(pos(i)))).toList
  }

  /** returns `true` if every element is equal to its index */
  def isIdentity(a: Array[Int]) = (0 until a.length).forall(i => a(i) == i)

  /**
   * Transform a function application into a Block, and assigns typer.context
   * .namedApplyBlockInfo to the new block as side-effect. If tree has the form
   *    Apply(fun, args)
   * first the function "fun" (which might be an application itself!) is transformed into a
   * block of the form
   *   {
   *     val qual$1 = qualifier_of_fun
   *     val x$1 = arg_1_of_fun
   *     ...
   *     val x$n = arg_n_of_fun
   *     qual$1.fun[targs](x$1, ...)...(..., x$n)
   *   }
   * then for each argument in args, a value is created and entered into the block. finally
   * the application expression of the block is updated.
   *   {
   *     val qual$1 = ..
   *     ...
   *     val x$n = ...
   *  >  val qual$n+1 = arg(1)
   *  >  ...
   *  >  val qual$n+m = arg(m)
   *  >  qual$1.fun[targs](x$1, ...)...(..., x$n)(x$n+1, ..., x$n+m)
   *   }
   *
   * @param typer the typer calling this method; this method calls
   *    typer.doTypedApply
   * @param mode the mode to use for calling typer.doTypedApply
   * @param pt the expected type for calling typer.doTypedApply
   *
   * @param tree: the function application tree
   * @argPos: a function mapping arguments from their current position to the
   *   position specified by the method type. example:
   *    def foo(a: Int, b: String)
   *    foo(b = "1", a = 2)
   *  calls
   *    transformNamedApplication(Apply(foo, List("1", 2), { 0 => 1, 1 => 0 })
   *
   *  @return the transformed application (a Block) together with the NamedApplyInfo.
   *     if isNamedApplyBlock(tree), returns the existing context.namedApplyBlockInfo
   */
  def transformNamedApplication(typer: Typer, mode: Int, pt: Type)
                               (tree: Tree, argPos: Int => Int): Tree = {
    import typer._
    import typer.infer._
    val context = typer.context
    import context.unit

    /**
     * Transform a function into a block, and passing context.namedApplyBlockInfo to
     * the new block as side-effect.
     *
     * `baseFun` is typed, the resulting block must be typed as well.
     *
     * Fun is transformed in the following way:
     *  - Ident(f)                                    ==>  Block(Nil, Ident(f))
     *  - Select(qual, f) if (qual is stable)         ==>  Block(Nil, Select(qual, f))
     *  - Select(qual, f) otherwise                   ==>  Block(ValDef(qual$1, qual), Select(qual$1, f))
     *  - TypeApply(fun, targs)                       ==>  Block(Nil or qual$1, TypeApply(fun, targs))
     *  - Select(New(TypeTree()), <init>)             ==>  Block(Nil, Select(New(TypeTree()), <init>))
     *  - Select(New(Select(qual, typeName)), <init>) ==>  Block(Nil, Select(...))     NOTE: qual must be stable in a `new`
     */
    def baseFunBlock(baseFun: Tree): Tree = {
      val isConstr = baseFun.symbol.isConstructor
      val blockTyper = newTyper(context.makeNewScope(tree, context.owner))

      // baseFun1: extract the function from a potential TypeApply
      // funTargs: type arguments on baseFun, used to reconstruct TypeApply in blockWith(Out)Qualifier
      // defaultTargs: type arguments to be used for calling defaultGetters. If the type arguments are given
      //   in the source code, re-use them for default getter. Otherwise infer the default getter's t-args.
      val (baseFun1, funTargs, defaultTargs) = baseFun match {
        case TypeApply(fun, targs) =>
          val targsInSource =
            if (targs.forall(a => context.undetparams contains a.symbol)) Nil
            else targs
          (fun, targs, targsInSource)

        case Select(New(tpt @ TypeTree()), _) if isConstr =>
          val targsInSource = tpt.tpe match {
            case TypeRef(pre, sym, args)
            if (!args.forall(a => context.undetparams contains a.typeSymbol)) =>
              args.map(TypeTree(_))
            case _ =>
              Nil
          }
          (baseFun, Nil, targsInSource)

        case Select(TypeApply(New(TypeTree()), targs), _) if isConstr =>
          val targsInSource =
            if (targs.forall(a => context.undetparams contains a.symbol)) Nil
            else targs
          (baseFun, Nil, targsInSource)

        case _ => (baseFun, Nil, Nil)
      }

      // never used for constructor calls, they always have a stable qualifier
      def blockWithQualifier(qual: Tree, selected: Name) = {
        val sym = blockTyper.context.owner.newValue(unit.freshTermName("qual$"), qual.pos, newFlags = ARTIFACT) setInfo qual.tpe
        blockTyper.context.scope enter sym
        val vd = atPos(sym.pos)(ValDef(sym, qual) setType NoType)
        // it stays in Vegas: SI-5720, SI-5727
        qual changeOwner (blockTyper.context.owner -> sym)

        val newQual = atPos(qual.pos.focus)(blockTyper.typedQualifier(Ident(sym.name)))
        val baseFunTransformed = atPos(baseFun.pos.makeTransparent) {
          // setSymbol below is important because the 'selected' function might be overloaded. by
          // assigning the correct method symbol, typedSelect will just assign the type. the reason
          // to still call 'typed' is to correctly infer singleton types, SI-5259.
          val f = blockTyper.typedOperator(Select(newQual, selected).setSymbol(baseFun1.symbol))
          if (funTargs.isEmpty) f
          else TypeApply(f, funTargs).setType(baseFun.tpe)
        }

        val b = Block(List(vd), baseFunTransformed)
                  .setType(baseFunTransformed.tpe).setPos(baseFun.pos)
        context.namedApplyBlockInfo =
          Some((b, NamedApplyInfo(Some(newQual), defaultTargs, Nil, blockTyper)))
        b
      }

      def blockWithoutQualifier(defaultQual: Option[Tree]) = {
        val b = atPos(baseFun.pos)(Block(Nil, baseFun).setType(baseFun.tpe))
        context.namedApplyBlockInfo =
          Some((b, NamedApplyInfo(defaultQual, defaultTargs, Nil, blockTyper)))
        b
      }

      def moduleQual(pos: Position, classType: Type) = {
        // prefix does 'normalize', which fixes #3384
        val pre = classType.prefix
        if (pre == NoType) {
          None
        } else {
          val module = companionSymbolOf(baseFun.symbol.owner, context)
          if (module == NoSymbol) None
          else {
            val ref = atPos(pos.focus)(gen.mkAttributedRef(pre, module))
            if (module.isStable && pre.isStable)    // fixes #4524. the type checker does the same for
              ref.setType(singleType(pre, module))  // typedSelect, it calls "stabilize" on the result.
            Some(ref)
          }
        }
      }

      baseFun1 match {
        // constructor calls

        case Select(New(tp @ TypeTree()), _) if isConstr =>
          // 'moduleQual' fixes #3338. Same qualifier for selecting the companion object as for the class.
          blockWithoutQualifier(moduleQual(tp.pos, tp.tpe))
        case Select(TypeApply(New(tp @ TypeTree()), _), _) if isConstr =>
          blockWithoutQualifier(moduleQual(tp.pos, tp.tpe))

        case Select(New(tp @ Ident(_)), _) if isConstr =>
          // 'moduleQual' fixes #3344
          blockWithoutQualifier(moduleQual(tp.pos, tp.tpe))
        case Select(TypeApply(New(tp @ Ident(_)), _), _) if isConstr =>
          blockWithoutQualifier(moduleQual(tp.pos, tp.tpe))

        case Select(New(tp @ Select(qual, _)), _) if isConstr =>
          // in `new q.C()', q is always stable
          assert(treeInfo.isExprSafeToInline(qual), qual)
          // 'moduleQual' fixes #2057
          blockWithoutQualifier(moduleQual(tp.pos, tp.tpe))
        case Select(TypeApply(New(tp @ Select(qual, _)), _), _) if isConstr =>
          assert(treeInfo.isExprSafeToInline(qual), qual)
          blockWithoutQualifier(moduleQual(tp.pos, tp.tpe))

        // super constructor calls
        case Select(sp @ Super(_, _), _) if isConstr =>
          // 'moduleQual' fixes #3207. selection of the companion module of the
          // superclass needs to have the same prefix as the superclass.
          blockWithoutQualifier(moduleQual(baseFun.pos, sp.symbol.tpe.firstParent))

        // self constructor calls (in secondary constructors)
        case Select(tp, name) if isConstr =>
          assert(treeInfo.isExprSafeToInline(tp), tp)
          blockWithoutQualifier(moduleQual(tp.pos, tp.tpe))

        // other method calls

        case Ident(_) =>
          blockWithoutQualifier(None)

        case Select(qual, name) =>
          if (treeInfo.isExprSafeToInline(qual))
            blockWithoutQualifier(Some(qual.duplicate))
          else
            blockWithQualifier(qual, name)
      }
    }

    /**
     * For each argument (arg: T), create a local value
     *  x$n: T = arg
     *
     * assumes "args" are typed. owner of the definitions in the block is the owner of
     * the block (see typedBlock), but the symbols have to be entered into the block's scope.
     *
     * For by-name parameters, create a value
     *  x$n: () => T = () => arg
     */
    def argValDefs(args: List[Tree], paramTypes: List[Type], blockTyper: Typer): List[ValDef] = {
      val context = blockTyper.context
      val symPs = map2(args, paramTypes)((arg, tpe) => {
        val byName   = isByNameParamType(tpe)
        val repeated = isScalaRepeatedParamType(tpe)
        val argTpe = (
          if (repeated) arg match {
            case Typed(expr, Ident(tpnme.WILDCARD_STAR)) => expr.tpe
            case _                                       => seqType(arg.tpe)
          }
          else arg.tpe
        ).widen // have to widen or types inferred from literal defaults will be singletons
        val s = context.owner.newValue(unit.freshTermName("x$"), arg.pos, newFlags = ARTIFACT) setInfo (
          if (byName) functionType(Nil, argTpe) else argTpe
        )
        (context.scope.enter(s), byName, repeated)
      })
      map2(symPs, args) {
        case ((sym, byName, repeated), arg) =>
          val body =
            if (byName) {
              val res = blockTyper.typed(Function(List(), arg))
              new ChangeOwnerTraverser(context.owner, res.symbol) traverse arg // fixes #2290
              res
            } else {
              new ChangeOwnerTraverser(context.owner, sym) traverse arg // fixes #4502
              if (repeated) arg match {
                case Typed(expr, Ident(tpnme.WILDCARD_STAR)) =>
                  expr
                case _ =>
                  val factory = Select(gen.mkAttributedRef(SeqModule), nme.apply)
                  blockTyper.typed(Apply(factory, List(resetLocalAttrs(arg))))
              } else arg
            }
          atPos(body.pos)(ValDef(sym, body).setType(NoType))
      }
    }

    // begin transform
    if (isNamedApplyBlock(tree)) {
      context.namedApplyBlockInfo.get._1
    } else tree match {
      // `fun` is typed. `namelessArgs` might be typed or not, if they are types are kept.
      case Apply(fun, namelessArgs) =>
        val transformedFun = transformNamedApplication(typer, mode, pt)(fun, x => x)
        if (transformedFun.isErroneous) setError(tree)
        else {
          assert(isNamedApplyBlock(transformedFun), transformedFun)
          val NamedApplyInfo(qual, targs, vargss, blockTyper) =
            context.namedApplyBlockInfo.get._2
          val Block(stats, funOnly) = transformedFun

          // type the application without names; put the arguments in definition-site order
          val typedApp = doTypedApply(tree, funOnly, reorderArgs(namelessArgs, argPos), mode, pt)
          if (typedApp.isErrorTyped) tree
          else typedApp match {
            // Extract the typed arguments, restore the call-site evaluation order (using
            // ValDef's in the block), change the arguments to these local values.
            case Apply(expr, typedArgs) =>
              // typedArgs: definition-site order
              val formals = formalTypes(expr.tpe.paramTypes, typedArgs.length, false, false)
              // valDefs: call-site order
              val valDefs = argValDefs(reorderArgsInv(typedArgs, argPos),
                                       reorderArgsInv(formals, argPos),
                                       blockTyper)
              // refArgs: definition-site order again
              val refArgs = map2(reorderArgs(valDefs, argPos), formals)((vDef, tpe) => {
                val ref = gen.mkAttributedRef(vDef.symbol)
                atPos(vDef.pos.focus) {
                  // for by-name parameters, the local value is a nullary function returning the argument
                  tpe.typeSymbol match {
                    case ByNameParamClass   => Apply(ref, Nil)
                    case RepeatedParamClass => Typed(ref, Ident(tpnme.WILDCARD_STAR))
                    case _                  => ref
                  }
                }
              })
              // cannot call blockTyper.typedBlock here, because the method expr might be partially applied only
              val res = blockTyper.doTypedApply(tree, expr, refArgs, mode, pt)
              res.setPos(res.pos.makeTransparent)
              val block = Block(stats ::: valDefs, res).setType(res.tpe).setPos(tree.pos.makeTransparent)
              context.namedApplyBlockInfo =
                Some((block, NamedApplyInfo(qual, targs, vargss :+ refArgs, blockTyper)))
              block
          }
        }

      case baseFun => // also treats "case TypeApply(fun, targs)" and "case Select(New(..), <init>)"
        baseFunBlock(baseFun)

    }
  }

  def missingParams[T](args: List[T], params: List[Symbol], argName: T => Option[Name] = nameOf _): (List[Symbol], Boolean) = {
    val namedArgs = args.dropWhile(arg => {
      val n = argName(arg)
      n.isEmpty || params.forall(p => p.name != n.get)
    })
    val namedParams = params.drop(args.length - namedArgs.length)
    // missing: keep those with a name which doesn't exist in namedArgs
    val missingParams = namedParams.filter(p => namedArgs.forall(arg => {
      val n = argName(arg)
      n.isEmpty || n.get != p.name
    }))
    val allPositional = missingParams.length == namedParams.length
    (missingParams, allPositional)
  }

  /**
   * Extend the argument list `givenArgs` with default arguments. Defaults are added
   * as named arguments calling the corresponding default getter.
   *
   * Example: given
   *   def foo(x: Int = 2, y: String = "def")
   *   foo(y = "lt")
   * the argument list (y = "lt") is transformed to (y = "lt", x = foo$default$1())
   */
  def addDefaults(givenArgs: List[Tree], qual: Option[Tree], targs: List[Tree],
                  previousArgss: List[List[Tree]], params: List[Symbol],
                  pos: scala.reflect.internal.util.Position, context: Context): (List[Tree], List[Symbol]) = {
    if (givenArgs.length < params.length) {
      val (missing, positional) = missingParams(givenArgs, params)
      if (missing forall (_.hasDefault)) {
        val defaultArgs = missing flatMap (p => {
          val defGetter = defaultGetter(p, context)
          // TODO #3649 can create spurious errors when companion object is gone (because it becomes unlinked from scope)
          if (defGetter == NoSymbol) None // prevent crash in erroneous trees, #3649
          else {
            var default1 = qual match {
              case Some(q) => gen.mkAttributedSelect(q.duplicate, defGetter)
              case None    => gen.mkAttributedRef(defGetter)

            }
            default1 = if (targs.isEmpty) default1
                       else TypeApply(default1, targs.map(_.duplicate))
            val default2 = (default1 /: previousArgss)((tree, args) =>
              Apply(tree, args.map(_.duplicate)))
            Some(atPos(pos) {
              if (positional) default2
              else AssignOrNamedArg(Ident(p.name), default2)
            })
          }
        })
        (givenArgs ::: defaultArgs, Nil)
      } else (givenArgs, missing filterNot (_.hasDefault))
    } else (givenArgs, Nil)
  }

  /**
   * For a parameter with default argument, find the method symbol of
   * the default getter.
   */
  def defaultGetter(param: Symbol, context: Context): Symbol = {
    val i = param.owner.paramss.flatten.indexWhere(p => p.name == param.name) + 1
    if (i > 0) {
      val defGetterName = nme.defaultGetterName(param.owner.name, i)
      if (param.owner.isConstructor) {
        val mod = companionSymbolOf(param.owner.owner, context)
        mod.info.member(defGetterName)
      }
      else {
        // isClass also works for methods in objects, owner is the ModuleClassSymbol
        if (param.owner.owner.isClass) {
          // .toInterface: otherwise we get the method symbol of the impl class
          param.owner.owner.toInterface.info.member(defGetterName)
        } else {
          // the owner of the method is another method. find the default
          // getter in the context.
          context.lookup(defGetterName, param.owner.owner)
        }
      }
    } else NoSymbol
  }

  private def savingUndeterminedTParams[T](context: Context)(fn: List[Symbol] => T): T = {
    val savedParams    = context.extractUndetparams()
    val savedReporting = context.ambiguousErrors

    context.setAmbiguousErrors(false)
    try fn(savedParams)
    finally {
      context.setAmbiguousErrors(savedReporting)
      //@M note that we don't get here when an ambiguity was detected (during the computation of res),
      // as errorTree throws an exception
      context.undetparams = savedParams
    }
  }

  /** A full type check is very expensive; let's make sure there's a name
   *  somewhere which could potentially be ambiguous before we go that route.
   */
  private def isAmbiguousAssignment(typer: Typer, param: Symbol, arg: Tree) = {
    import typer.context
    (context isNameInScope param.name) && {
      // for named arguments, check whether the assignment expression would
      // typecheck. if it does, report an ambiguous error.
      val paramtpe = param.tpe.cloneInfo(param)
      // replace type parameters by wildcard. in the below example we need to
      // typecheck (x = 1) with wildcard (not T) so that it succeeds.
      //   def f[T](x: T) = x
      //   var x = 0
      //   f(x = 1)   <<  "x = 1" typechecks with expected type WildcardType
      savingUndeterminedTParams(context) { udp =>
        val subst = new SubstTypeMap(udp, udp map (_ => WildcardType)) {
          override def apply(tp: Type): Type = super.apply(tp match {
            case TypeRef(_, ByNameParamClass, x :: Nil) => x
            case _                                      => tp
          })
        }
        // This throws an exception which is caught in `tryTypedApply` (as it
        // uses `silent`) - unfortunately, tryTypedApply recovers from the
        // exception if you use errorTree(arg, ...) and conforms is allowed as
        // a view (see tryImplicit in Implicits) because it tries to produce a
        // new qualifier (if the old one was P, the new one will be
        // conforms.apply(P)), and if that works, it pretends nothing happened.
        //
        // To make sure tryTypedApply fails, we would like to pass EmptyTree
        // instead of arg, but can't do that because eventually setType(ErrorType)
        // is called, and EmptyTree can only be typed NoType.  Thus we need to
        // disable conforms as a view...
        val errsBefore = reporter.ERROR.count
        try typer.silent { tpr =>
          val res = tpr.typed(arg, subst(paramtpe))
          // better warning for SI-5044: if `silent` was not actually silent give a hint to the user
          // [H]: the reason why `silent` is not silent is because the cyclic reference exception is
          // thrown in a context completely different from `context` here. The exception happens while
          // completing the type, and TypeCompleter is created/run with a non-silent Namer `context`
          // and there is at the moment no way to connect the two unless we go through some global state.
          if (errsBefore < reporter.ERROR.count)
            WarnAfterNonSilentRecursiveInference(param, arg)(context)
          res
        } match {
          case SilentResultValue(t)  => !t.isErroneous // #4041
          case _        => false
        }
        catch {
          // `silent` only catches and returns TypeErrors which are not
          // CyclicReferences.  Fix for #3685
          case cr @ CyclicReference(sym, _) =>
            (sym.name == param.name) && sym.accessedOrSelf.isVariable && {
              NameClashError(sym, arg)(context)
              true
            }
        }
      }
    }
  }

  /**
   * Removes name assignments from args. Additionally, returns an array mapping
   * argument indices from call-site-order to definition-site-order.
   *
   * Verifies that names are not specified twice, positional args don't appear
   * after named ones.
   */
  def removeNames(typer: Typer)(args: List[Tree], params: List[Symbol]): (List[Tree], Array[Int]) = {
    implicit val context0 = typer.context
    // maps indices from (order written by user) to (order of definition)
    val argPos            = Array.fill(args.length)(-1)
    var positionalAllowed = true
    val namelessArgs = mapWithIndex(args) { (arg, argIndex) =>
      arg match {
        case arg @ AssignOrNamedArg(Ident(name), rhs) =>
          def matchesName(param: Symbol) = !param.isSynthetic && (
            (param.name == name) || (param.deprecatedParamName match {
              case Some(`name`) =>
                context0.unit.deprecationWarning(arg.pos,
                  "the parameter name "+ name +" has been deprecated. Use "+ param.name +" instead.")
                true
              case _ => false
            })
          )
          val paramPos = params indexWhere matchesName
          if (paramPos == -1) {
            if (positionalAllowed) {
              argPos(argIndex) = argIndex
              // prevent isNamed from being true when calling doTypedApply recursively,
              // treat the arg as an assignment of type Unit
              Assign(arg.lhs, rhs) setPos arg.pos
            }
            else UnknownParameterNameNamesDefaultError(arg, name)
          }
          else if (argPos contains paramPos) {
            val existingArgIndex = argPos.indexWhere(_ == paramPos)
            val otherName = args(paramPos) match {
              case AssignOrNamedArg(Ident(oName), rhs) if oName != name => Some(oName)
              case _ => None
            }
            DoubleParamNamesDefaultError(arg, name, existingArgIndex+1, otherName)
          } else if (isAmbiguousAssignment(typer, params(paramPos), arg))
            AmbiguousReferenceInNamesDefaultError(arg, name)
          else {
            // if the named argument is on the original parameter
            // position, positional after named is allowed.
            if (argIndex != paramPos)
              positionalAllowed = false
            argPos(argIndex) = paramPos
            rhs
          }
        case _ =>
          argPos(argIndex) = argIndex
          if (positionalAllowed) arg
          else PositionalAfterNamedNamesDefaultError(arg)
      }
    }

    (namelessArgs, argPos)
  }
}
