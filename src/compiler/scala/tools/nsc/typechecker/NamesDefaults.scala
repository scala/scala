/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id: NamesDefaults.scala 17081 2009-02-10 17:45:38Z rytz $

package scala.tools.nsc.typechecker

import scala.collection.mutable.ListBuffer
  import symtab.Flags._

/**
 *  @author Lukas Rytz
 *  @version 1.0
 */
trait NamesDefaults { self: Analyzer =>

  import global._
  import definitions._

  case class NamedApplyInfo(qual: Option[Tree], targs: List[Tree],
                            vargss: List[List[Tree]], blockTyper: Typer)
  val noApplyInfo = NamedApplyInfo(None, Nil, Nil, null)

  def nameOf(arg: Tree) = arg match {
    case a @ Assign(Ident(name), rhs) if a.namedArg => Some(name)
    case _ => None
  }
  def isNamed(arg: Tree) = nameOf(arg).isDefined


  /** @param pos maps indicies from old to new */
  def reorderArgs[T](args: List[T], pos: Int => Int): List[T] = {
    val res = new Array[T](args.length)
    // (hopefully) faster than zipWithIndex
    (0 /: args) { case (index, arg) => res(pos(index)) = arg; index + 1 }
    res.toList
  }

  /** @param pos maps indicies from new to old (!) */
  def reorderArgsInv[T](args: List[T], pos: Int => Int): List[T] = {
    val argsArray = args.toArray
    val res = new ListBuffer[T]
    for (i <- 0 until argsArray.length)
      res += argsArray(pos(i))
    res.toList
  }

  /** returns `true' if every element is equal to its index */
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
     * Transform a function into a block, and assing context.namedApplyBlockInfo to
     * the new block as side-effect.
     * Fun is transformed in the following way:
     *  - Ident(f)                             ==>  Block(Nil, Ident(f))
     *  - Select(qual, f) if (qual is stable)  ==>  Block(Nil, Select(qual, f))
     *  - Select(qual, f) otherwise            ==>  Block(ValDef(qual$1, qual), Select(qual$1, f))
     *  - TypeApply(fun, targs)                ==>  Block(Nil or qual$1, TypeApply(fun, targs))
     *  - Select(New(TypeTree()), <init>)      ==>  Block(Nil, Select(New(TypeTree()), <init>))
     *  - Select(New(Select(qual, typeName)), <init>) if (qual is stable)
     *       ==>  Block(Nil, Select(...))
     *  - Select(New(Select(qual, typeName)), <init>) otherwise
     *       ==>  Block(ValDef(qual$1, qual), Select(New(Select(qual$1, typeName)), <init>))
     */
    def baseFunBlock(baseFun: Tree): Tree = {
      val isConstr = baseFun.symbol.isConstructor
      val blockTyper = newTyper(context.makeNewScope(tree, context.owner)(BlockScopeKind(context.depth)))

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
            case _ => Nil
          }
          (baseFun, Nil, targsInSource)

        case _ => (baseFun, Nil, Nil)
      }

      def blockWithQualifier(qual: Tree, fun: Symbol => Tree, defaultQual: Symbol => Option[Tree]) = {
        val sym = blockTyper.context.owner.newValue(baseFun1.pos,
                                                    unit.fresh.newName(baseFun1.pos, "qual$"))
                            .setInfo(qual.tpe)
        blockTyper.context.scope.enter(sym)
        val vd = atPos(sym.pos) { ValDef(sym, qual).setType(NoType) }
        var baseFunTransformed: Tree = fun(sym)
        if (!funTargs.isEmpty)
          baseFunTransformed = treeCopy.TypeApply(baseFun, baseFunTransformed, funTargs)
        val b = atPos(baseFun1.pos) { Block(List(vd), baseFunTransformed)
                                      .setType(baseFunTransformed.tpe) }
        context.namedApplyBlockInfo =
          Some((b, NamedApplyInfo(defaultQual(sym), defaultTargs, Nil, blockTyper)))
        b
      }

      def blockWithoutQualifier(fun: Tree, defaultQual: Option[Tree]) = {
       val fun1 = if (funTargs.isEmpty) fun
                  else treeCopy.TypeApply(baseFun, fun, funTargs)
        val b = atPos(baseFun.pos) { Block(Nil, fun1).setType(fun1.tpe) }
        context.namedApplyBlockInfo =
          Some((b, NamedApplyInfo(defaultQual, defaultTargs, Nil, blockTyper)))
        b
      }

      baseFun1 match {
        // constructor calls

        case Select(New(TypeTree()), _) if isConstr =>
          blockWithoutQualifier(baseFun1, None)

        case Select(New(Ident(_)), _) if isConstr =>
          blockWithoutQualifier(baseFun1, None)

        case Select(nev @ New(sel @ Select(qual, typeName)), constr) if isConstr =>
          // #2057
          val module = baseFun.symbol.owner.linkedModuleOfClass
          val defaultQual = if (module == NoSymbol) None
                            else Some(gen.mkAttributedSelect(qual.duplicate, module))
          // in `new q.C()', q is always stable
          assert(treeInfo.isPureExpr(qual), qual)
          blockWithoutQualifier(baseFun1, defaultQual)

        // super constructor calls

        case Select(Super(_, _), _) if isConstr =>
          blockWithoutQualifier(baseFun1, None)

        // self constructor calls (in secondary constructors)

        case Select(qual, name) if isConstr =>
          assert(treeInfo.isPureExpr(qual), qual)
          blockWithoutQualifier(baseFun1, None)

        // other method calls

        case Ident(_) =>
          blockWithoutQualifier(baseFun1, None)

        case Select(qual, name) =>
          if (treeInfo.isPureExpr(qual))
            blockWithoutQualifier(baseFun1, Some(qual.duplicate))
          else
            blockWithQualifier(qual,
                               sym => treeCopy.Select(baseFun1, gen.mkAttributedRef(sym), name),
                               sym => Some(gen.mkAttributedRef(sym)))
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
      val symPs = List.map2(args, paramTypes)((arg, tpe) => {
        val byName = tpe.typeSymbol == ByNameParamClass
        val s = context.owner.newValue(arg.pos, unit.fresh.newName(arg.pos, "x$"))
        val valType = if (byName) functionType(List(), arg.tpe)
                  else arg.tpe
        s.setInfo(valType)
        (context.scope.enter(s), byName)
      })
      List.map2(symPs, args)((symP, arg) => {
        val (sym, byName) = symP
        val body = if (byName) blockTyper.typed(Function(List(), arg))
                   else arg
        ValDef(sym, body).setType(NoType)
      })
    }

    // begin transform
    if (isNamedApplyBlock(tree)) {
      context.namedApplyBlockInfo.get._1
    } else tree match {
      // we know that Apply(Apply(...)) can only be an application of a curried method;
      // for functions, it's transformed to applying the .apply() method already.
      case Apply(fun, namelessArgs) =>
        val transformedFun = transformNamedApplication(typer, mode, pt)(fun, x => x)
        if (transformedFun.isErroneous) setError(tree)
        else {
          assert(isNamedApplyBlock(transformedFun), transformedFun)
          val NamedApplyInfo(qual, targs, vargss, blockTyper) =
            context.namedApplyBlockInfo.get._2
          val existingBlock @ Block(stats, funOnly) = transformedFun

          // type the application without names; put the arguments in definition-site order
          val typedApp = doTypedApply(tree, funOnly, reorderArgs(namelessArgs, argPos), mode, pt)

          if (typedApp.tpe.isError) setError(tree)
          else typedApp match {
            // Extract the typed arguments, restore the call-site evaluation order (using
            // ValDef's in the block), change the arguments to these local values.
            case Apply(expr, typedArgs) =>
              // typedArgs: definition-site order
              val formals = formalTypes(expr.tpe.paramTypes, typedArgs.length, false)
              // valDefs: call-site order
              val valDefs = argValDefs(reorderArgsInv(typedArgs, argPos),
                                       reorderArgsInv(formals, argPos),
                                       blockTyper)
              // refArgs: definition-site order again
              val refArgs = List.map2(reorderArgs(valDefs, argPos), formals)((vDef, tpe) => {
                val ref = gen.mkAttributedRef(vDef.symbol)
                // for by-name parameters, the local value is a nullary function returning the argument
                if (tpe.typeSymbol == ByNameParamClass) Apply(ref, List())
                else ref
              })
              // cannot call blockTyper.typedBlock here, because the method expr might be partially applied only
              val res = blockTyper.doTypedApply(tree, expr, refArgs, mode, pt)
              val block = treeCopy.Block(existingBlock, stats ::: valDefs, res).setType(res.tpe)
              context.namedApplyBlockInfo =
                Some((block, NamedApplyInfo(qual, targs, vargss ::: List(refArgs), blockTyper)))
              block
          }
        }

      // case ApplyDynamic??? case AppliedTypeTree???

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
   * Extend the argument list `givenArgs' with default arguments. Defaults are added
   * as named arguments calling the corresponding default getter.
   *
   * Example: given
   *   def foo(x: Int = 2, y: String = "def")
   *   foo(1)
   * the argument list (y = "lt") is transformed to (y = "lt", x = foo$default$1())
   */
  def addDefaults(givenArgs: List[Tree], qual: Option[Tree], targs: List[Tree],
                  previousArgss: List[List[Tree]], params: List[Symbol], pos: util.Position): (List[Tree], List[Symbol]) = {
    if (givenArgs.length < params.length) {
      val (missing, positional) = missingParams(givenArgs, params)
      if (missing forall (_.hasFlag(DEFAULTPARAM))) {
        val defaultArgs = missing map (p => {
          var default1 = qual match {
            case Some(q) => gen.mkAttributedSelect(q.duplicate, p.defaultGetter)
            case None => gen.mkAttributedRef(p.defaultGetter)
          }
          default1 = if (targs.isEmpty) default1
                     else TypeApply(default1, targs.map(_.duplicate)).setPos(pos)
          val default2 = (default1 /: previousArgss)((tree, args) =>
            Apply(tree, args.map(_.duplicate)).setPos(pos))
          if (positional) default2
          else atPos(pos) { new AssignOrNamedArg(Ident(p.name), default2) }
        })
        (givenArgs ::: defaultArgs, Nil)
      } else (givenArgs, missing filter (! _.hasFlag(DEFAULTPARAM)))
    } else (givenArgs, Nil)
  }

  /**
   * Removes name assignments from args. Additionally, returns an array mapping
   * argument indicies from call-site-order to definition-site-order.
   *
   * Verifies that names are not specified twice, positional args don't appear
   * after named ones.
   */
  def removeNames(typer: Typer)(args: List[Tree], params: List[Symbol]): (List[Tree], Array[Int]) = {
    import typer.infer.errorTree

    // maps indicies from (order written by user) to (order of definition)
    val argPos = (new Array[Int](args.length)) map (x => -1)
    var positionalAllowed = true
    val namelessArgs = for ((arg, index) <- (args.zipWithIndex)) yield arg match {
      case a @ Assign(Ident(name), rhs) if a.namedArg =>
        val pos = params.indexWhere(p => p.name == name && !p.hasFlag(SYNTHETIC))
        if (pos == -1) {
          if (positionalAllowed) {
            argPos(index) = index
            // prevent isNamed from being true when calling doTypedApply recursively,
            // treat the arg as an assignment of type Unit
            Assign(a.lhs, rhs).setPos(arg.pos)
          } else {
            errorTree(arg, "unknown parameter name: "+ name)
          }
        } else if (argPos contains pos) {
          errorTree(arg, "parameter specified twice: "+ name)
        } else {
          // for named arguments, check wether the assignment expression would
          // typecheck. if it does, report an ambiguous error.
          val param = params(pos)
          val paramtpe = params(pos).tpe.cloneInfo(param)
          // replace type parameters by wildcard. in the below example we need to
          // typecheck (x = 1) with wildcard (not T) so that it succeeds.
          //   def f[T](x: T) = x
          //   var x = 0
          //   f(x = 1)   <<  "x = 1" typechecks with expected type WildcardType
          val udp = typer.context.extractUndetparams()
          val subst = new SubstTypeMap(udp, udp map (_ => WildcardType))
          val res = typer.silent(_.typed(arg, subst(paramtpe))) match {
            case _: TypeError =>
              positionalAllowed = false
              argPos(index) = pos
              rhs
            case t: Tree =>
              errorTree(arg, "reference to "+ name +" is ambiguous; it is both, a parameter\n"+
                             "name of the method and the name of a variable currently in scope.")
          }
          typer.context.undetparams = udp
          res
        }
      case _ =>
        argPos(index) = index
        if (positionalAllowed) arg
        else errorTree(arg, "positional after named argument.")
    }
    (namelessArgs, argPos)
  }
}
