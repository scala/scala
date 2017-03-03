/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package transform

import symtab._
import Flags._
import scala.collection._
import scala.language.postfixOps

abstract class CleanUp extends Statics with Transform with ast.TreeDSL {
  import global._
  import definitions._
  import CODE._
  import treeInfo.StripCast

  /** the following two members override abstract members in Transform */
  val phaseName: String = "cleanup"

  /* used in GenBCode: collects ClassDef symbols owning a main(Array[String]) method */
  private var entryPoints: List[Symbol] = Nil
  def getEntryPoints: List[Symbol] = entryPoints sortBy ("" + _.fullName) // For predictably ordered error messages.

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new CleanUpTransformer(unit)

  class CleanUpTransformer(unit: CompilationUnit) extends StaticsTransformer {
    private val newStaticMembers      = mutable.Buffer.empty[Tree]
    private val newStaticInits        = mutable.Buffer.empty[Tree]
    private val symbolsStoredAsStatic = mutable.Map.empty[String, Symbol]
    private def clearStatics() {
      newStaticMembers.clear()
      newStaticInits.clear()
      symbolsStoredAsStatic.clear()
    }
    private def transformTemplate(tree: Tree) = {
      val Template(_, _, body) = tree
      clearStatics()
      val newBody = transformTrees(body)
      val templ   = deriveTemplate(tree)(_ => transformTrees(newStaticMembers.toList) ::: newBody)
      try
        if (newStaticInits.isEmpty) templ
        else deriveTemplate(templ)(body => staticConstructor(body, localTyper, templ.pos)(newStaticInits.toList) :: body)
      finally clearStatics()
    }
    private def mkTerm(prefix: String): TermName = unit.freshTermName(prefix)

    //private val classConstantMeth = new HashMap[String, Symbol]
    //private val symbolStaticFields = new HashMap[String, (Symbol, Tree, Tree)]

    private var localTyper: analyzer.Typer = null

    private def typedWithPos(pos: Position)(tree: Tree) =
      localTyper.typedPos(pos)(tree)

    /** A value class is defined to be only Java-compatible values: unit is
      * not part of it, as opposed to isPrimitiveValueClass in definitions. scala.Int is
      * a value class, java.lang.Integer is not. */
    def isJavaValueClass(sym: Symbol) = boxedClass contains sym
    def isJavaValueType(tp: Type) = isJavaValueClass(tp.typeSymbol)

    /** The boxed type if it's a primitive; identity otherwise.
     */
    def toBoxedType(tp: Type) = if (isJavaValueType(tp)) boxedClass(tp.typeSymbol).tpe else tp

    def transformApplyDynamic(ad: ApplyDynamic) = {
      val qual0 = ad.qual
      val params = ad.args
        if (settings.logReflectiveCalls)
          reporter.echo(ad.pos, "method invocation uses reflection")

        val typedPos = typedWithPos(ad.pos) _

        assert(ad.symbol.isPublic)
        var qual: Tree = qual0

        /* ### CREATING THE METHOD CACHE ### */

        def addStaticMethodToClass(forBody: (Symbol, Symbol) => Tree): Symbol = {
          val methSym = currentClass.newMethod(mkTerm(nme.reflMethodName.toString), ad.pos, STATIC | SYNTHETIC)
          val params  = methSym.newSyntheticValueParams(List(ClassClass.tpe))
          methSym setInfoAndEnter MethodType(params, MethodClass.tpe)

          val methDef = typedPos(DefDef(methSym, forBody(methSym, params.head)))
          newStaticMembers append transform(methDef)
          methSym
        }

        def reflectiveMethodCache(method: String, paramTypes: List[Type]): Symbol = {
          /* Implementation of the cache is as follows for method "def xyz(a: A, b: B)"
             (SoftReference so that it does not interfere with classloader garbage collection,
             see ticket #2365 for details):

            var reflParams$Cache: Array[Class[_]] = Array[JClass](classOf[A], classOf[B])

            var reflPoly$Cache: SoftReference[scala.runtime.MethodCache] = new SoftReference(new EmptyMethodCache())

            def reflMethod$Method(forReceiver: JClass[_]): JMethod = {
              var methodCache: StructuralCallSite = indy[StructuralCallSite.bootstrap, "(LA;LB;)Ljava/lang/Object;]
              if (methodCache eq null) {
                methodCache = new EmptyMethodCache
                reflPoly$Cache = new SoftReference(methodCache)
              }
              var method: JMethod = methodCache.find(forReceiver)
              if (method ne null)
                return method
              else {
                method = ScalaRunTime.ensureAccessible(forReceiver.getMethod("xyz", methodCache.parameterTypes()))
                methodCache.add(forReceiver, method)
                return method
              }
            }

            invokedynamic is used rather than a static field for the cache to support emitting bodies of methods
            in Java 8 interfaces, which don't support private static fields.
          */

          addStaticMethodToClass((reflMethodSym, forReceiverSym) => {
            val methodCache = reflMethodSym.newVariable(mkTerm("methodCache"), ad.pos) setInfo StructuralCallSite.tpe
            val methodSym = reflMethodSym.newVariable(mkTerm("method"), ad.pos) setInfo MethodClass.tpe

            val dummyMethodType = MethodType(NoSymbol.newSyntheticValueParams(paramTypes), AnyTpe)
            BLOCK(
              ValDef(methodCache, ApplyDynamic(gen.mkAttributedIdent(StructuralCallSite_dummy), LIT(StructuralCallSite_bootstrap) :: LIT(dummyMethodType) :: Nil).setType(StructuralCallSite.tpe)),
              ValDef(methodSym, (REF(methodCache) DOT StructuralCallSite_find)(REF(forReceiverSym))),
              IF (REF(methodSym) OBJ_NE NULL) .
                THEN (Return(REF(methodSym)))
              ELSE {
                def methodSymRHS  = ((REF(forReceiverSym) DOT Class_getMethod)(LIT(method), (REF(methodCache) DOT StructuralCallSite_getParameterTypes)()))
                def cacheAdd      = ((REF(methodCache) DOT StructuralCallSite_add)(REF(forReceiverSym), REF(methodSym)))
                BLOCK(
                  REF(methodSym)        === (REF(currentRun.runDefinitions.ensureAccessibleMethod) APPLY (methodSymRHS)),
                  cacheAdd,
                  Return(REF(methodSym))
                )
              }
            )
          })
        }

        /* ### HANDLING METHODS NORMALLY COMPILED TO OPERATORS ### */

        def testForName(name: Name): Tree => Tree = t => (
          if (nme.CommonOpNames(name))
            gen.mkMethodCall(currentRun.runDefinitions.Boxes_isNumberOrBool, t :: Nil)
          else if (nme.BooleanOpNames(name))
            t IS_OBJ BoxedBooleanClass.tpe
          else
            gen.mkMethodCall(currentRun.runDefinitions.Boxes_isNumber, t :: Nil)
        )

        /*  The Tree => Tree function in the return is necessary to prevent the original qual
         *  from being duplicated in the resulting code.  It may be a side-effecting expression,
         *  so all the test logic is routed through gen.evalOnce, which creates a block like
         *    { val x$1 = qual; if (x$1.foo || x$1.bar) f1(x$1) else f2(x$1) }
         *  (If the compiler can verify qual is safe to inline, it will not create the block.)
         */
        def getPrimitiveReplacementForStructuralCall(name: Name): Option[(Symbol, Tree => Tree)] = {
          val methodName = (
            if (params.isEmpty) nme.primitivePostfixMethodName(name)
            else if (params.tail.isEmpty) nme.primitiveInfixMethodName(name)
            else nme.NO_NAME
          )
          getDeclIfDefined(BoxesRunTimeClass, methodName) match {
            case NoSymbol => None
            case sym      => assert(!sym.isOverloaded, sym) ; Some((sym, testForName(name)))
          }
        }

        /* ### BOXING PARAMS & UNBOXING RESULTS ### */

        /* Transforms the result of a reflective call (always an AnyRef) to
         * the actual result value (an AnyRef too). The transformation
         * depends on the method's static return type.
         * - for units (void), the reflective call will return null: a new
         *   boxed unit is generated.
         * - otherwise, the value is simply casted to the expected type. This
         *   is enough even for value (int et al.) values as the result of
         *   a dynamic call will box them as a side-effect. */

        /* ### CALLING THE APPLY ### */
        def callAsReflective(paramTypes: List[Type], resType: Type): Tree = {
          val runDefinitions = currentRun.runDefinitions
          import runDefinitions._

          gen.evalOnce(qual, currentOwner, unit) { qual1 =>
            /* Some info about the type of the method being called. */
            val methSym       = ad.symbol
            val boxedResType  = toBoxedType(resType)      // Int -> Integer
            val resultSym     = boxedResType.typeSymbol
            // If this is a primitive method type (like '+' in 5+5=10) then the
            // parameter types and the (unboxed) result type should all be primitive types,
            // and the method name should be in the primitive->structural map.
            def isJavaValueMethod = (
              (resType :: paramTypes forall isJavaValueType) && // issue #1110
              (getPrimitiveReplacementForStructuralCall(methSym.name).isDefined)
            )
            // Erasure lets Unit through as Unit, but a method returning Any will have an
            // erased return type of Object and should also allow Unit.
            def isDefinitelyUnit  = (resultSym == UnitClass)
            def isMaybeUnit       = (resultSym == ObjectClass) || isDefinitelyUnit
            // If there's any chance this signature could be met by an Array.
            val isArrayMethodSignature = {
              def typesMatchApply = paramTypes match {
                case List(tp) => tp <:< IntTpe
                case _        => false
              }
              def typesMatchUpdate = paramTypes match {
                case List(tp1, tp2) => (tp1 <:< IntTpe) && isMaybeUnit
                case _              => false
              }

              (methSym.name == nme.length && params.isEmpty) ||
              (methSym.name == nme.clone_ && params.isEmpty) ||
              (methSym.name == nme.apply  && typesMatchApply) ||
              (methSym.name == nme.update && typesMatchUpdate)
            }

            /* Some info about the argument at the call site. */
            val qualSym           = qual.tpe.typeSymbol
            val args              = qual1() :: params
            def isDefinitelyArray = (qualSym == ArrayClass)
            def isMaybeArray      = (qualSym == ObjectClass) || isDefinitelyArray
            def isMaybeBoxed      = platform isMaybeBoxed qualSym

            // This is complicated a bit by trying to handle Arrays correctly.
            // Under normal circumstances if the erased return type is Object then
            // we're not going to box it to Unit, but that is the situation with
            // a signature like def f(x: { def update(x: Int, y: Long): Any })
            //
            // However we only want to do that boxing if it has been determined
            // to be an Array and a method returning Unit.  But for this fixResult
            // could be called in one place: instead it is called separately from the
            // unconditional outcomes (genValueCall, genArrayCall, genDefaultCall.)
            def fixResult(tree: Tree, mustBeUnit: Boolean = false) =
              if (mustBeUnit || resultSym == UnitClass) BLOCK(tree, REF(BoxedUnit_UNIT))  // boxed unit
              else if (resultSym == ObjectClass) tree                                     // no cast necessary
              else gen.mkCast(tree, boxedResType)                                         // cast to expected type

            /* Normal non-Array call */
            def genDefaultCall = {
              // reflective method call machinery
              val invokeName  = MethodClass.tpe member nme.invoke_                                  // scala.reflect.Method.invoke(...)
              def cache       = REF(reflectiveMethodCache(ad.symbol.name.toString, paramTypes))     // cache Symbol
              def lookup      = Apply(cache, List(qual1() GETCLASS()))                                // get Method object from cache
              def invokeArgs  = ArrayValue(TypeTree(ObjectTpe), params)                       // args for invocation
              def invocation  = (lookup DOT invokeName)(qual1(), invokeArgs)                        // .invoke(qual1, ...)

              // exception catching machinery
              val invokeExc   = currentOwner.newValue(mkTerm(""), ad.pos) setInfo InvocationTargetExceptionClass.tpe
              def catchVar    = Bind(invokeExc, Typed(Ident(nme.WILDCARD), TypeTree(InvocationTargetExceptionClass.tpe)))
              def catchBody   = Throw(Apply(Select(Ident(invokeExc), nme.getCause), Nil))

              // try { method.invoke } catch { case e: InvocationTargetExceptionClass => throw e.getCause() }
              fixResult(TRY (invocation) CATCH { CASE (catchVar) ==> catchBody } ENDTRY)
            }

            /* A possible primitive method call, represented by methods in BoxesRunTime. */
            def genValueCall(operator: Symbol) = fixResult(REF(operator) APPLY args)
            def genValueCallWithTest = {
              getPrimitiveReplacementForStructuralCall(methSym.name) match {
                case Some((operator, test)) =>
                  IF (test(qual1())) THEN genValueCall(operator) ELSE genDefaultCall
                case _ =>
                  genDefaultCall
              }
            }

            /* A native Array call. */
            def genArrayCall = fixResult(
              methSym.name match {
                case nme.length => REF(boxMethod(IntClass)) APPLY (REF(arrayLengthMethod) APPLY args)
                case nme.update => REF(arrayUpdateMethod) APPLY List(args(0), (REF(unboxMethod(IntClass)) APPLY args(1)), args(2))
                case nme.apply  => REF(arrayApplyMethod) APPLY List(args(0), (REF(unboxMethod(IntClass)) APPLY args(1)))
                case nme.clone_ => REF(arrayCloneMethod) APPLY List(args(0))
              },
              mustBeUnit = methSym.name == nme.update
            )

            /* A conditional Array call, when we can't determine statically if the argument is
             * an Array, but the structural type method signature is consistent with an Array method
             * so we have to generate both kinds of code.
             */
            def genArrayCallWithTest =
              IF ((qual1() GETCLASS()) DOT nme.isArray) THEN genArrayCall ELSE genDefaultCall

            localTyper typed (
              if (isMaybeBoxed && isJavaValueMethod) genValueCallWithTest
              else if (isArrayMethodSignature && isDefinitelyArray) genArrayCall
              else if (isArrayMethodSignature && isMaybeArray) genArrayCallWithTest
              else genDefaultCall
            )
          }
        }

        {

        /* ### BODY OF THE TRANSFORMATION -> remember we're in case ad@ApplyDynamic(qual, params) ### */

        /* This creates the tree that does the reflective call (see general comment
         * on the apply-dynamic tree for its format). This tree is simply composed
         * of three successive calls, first to getClass on the callee, then to
         * getMethod on the class, then to invoke on the method.
         * - getMethod needs an array of classes for choosing one amongst many
         *   overloaded versions of the method. This is provided by paramTypeClasses
         *   and must be done on the static type as Scala's dispatching is static on
         *   the parameters.
         * - invoke needs an array of AnyRefs that are the method's arguments. The
         *   erasure phase guarantees that any parameter passed to a dynamic apply
         *   is compatible (through boxing). Boxed ints et al. is what invoke expects
         *   when the applied method expects ints, hence no change needed there.
         * - in the end, the result of invoke must be fixed, again to deal with arrays.
         *   This is provided by fixResult. fixResult will cast the invocation's result
         *   to the method's return type, which is generally ok, except when this type
         *   is a value type (int et al.) in which case it must cast to the boxed version
         *   because invoke only returns object and erasure made sure the result is
         *   expected to be an AnyRef. */
        val t: Tree = {
          val (mparams, resType) = ad.symbol.tpe match {
            case MethodType(mparams, resType) =>
              assert(params.length == mparams.length, ((params, mparams)))
              (mparams, resType)
            case tpe @ OverloadedType(pre, alts) =>
              reporter.warning(ad.pos, s"Overloaded type reached the backend! This is a bug in scalac.\n     Symbol: ${ad.symbol}\n  Overloads: $tpe\n  Arguments: " + ad.args.map(_.tpe))
              alts filter (_.paramss.flatten.size == params.length) map (_.tpe) match {
                case mt @ MethodType(mparams, resType) :: Nil =>
                  reporter.warning(NoPosition, "Only one overload has the right arity, proceeding with overload " + mt)
                  (mparams, resType)
                case _ =>
                  reporter.error(ad.pos, "Cannot resolve overload.")
                  (Nil, NoType)
              }
            case NoType =>
              abort(ad.symbol.toString)
          }
          typedPos {
            val sym = currentOwner.newValue(mkTerm("qual"), ad.pos) setInfo qual0.tpe
            qual = REF(sym)

            BLOCK(
              ValDef(sym, qual0),
              callAsReflective(mparams map (_.tpe), resType)
            )
          }
        }

        /* For testing purposes, the dynamic application's condition
         * can be printed-out in great detail. Remove? */
        if (settings.debug) {
          def paramsToString(xs: Any*) = xs map (_.toString) mkString ", "
          val mstr = ad.symbol.tpe match {
            case MethodType(mparams, resType) =>
              sm"""|  with
                   |  - declared parameter types: '${paramsToString(mparams)}'
                   |  - passed argument types:    '${paramsToString(params)}'
                   |  - result type:              '${resType.toString}'"""
            case _ => ""
          }
          log(s"""Dynamically application '$qual.${ad.symbol.name}(${paramsToString(params)})' $mstr - resulting code: '$t'""")
        }

        /* We return the dynamic call tree, after making sure no other
         * clean-up transformation are to be applied on it. */
        transform(t)
      /* ### END OF DYNAMIC APPLY TRANSFORM ### */
      }
    }

    override def transform(tree: Tree): Tree = tree match {
      case _: ClassDef if genBCode.isJavaEntryPoint(tree.symbol, currentUnit) =>
        // collecting symbols for entry points here (as opposed to GenBCode where they are used)
        // has the advantage of saving an additional pass over all ClassDefs.
        entryPoints ::= tree.symbol
        super.transform(tree)

      /* Transforms dynamic calls (i.e. calls to methods that are undefined
       * in the erased type space) to -- dynamically -- unsafe calls using
       * reflection. This is used for structural sub-typing of refinement
       * types, but may be used for other dynamic calls in the future.
       * For 'a.f(b)' it will generate something like:
       * 'a.getClass().
       * '  getMethod("f", Array(classOf[b.type])).
       * '  invoke(a, Array(b))
       * plus all the necessary casting/boxing/etc. machinery required
       * for type-compatibility (see fixResult).
       *
       * USAGE CONTRACT:
       * There are a number of assumptions made on the way a dynamic apply
       * is used. Assumptions relative to type are handled by the erasure
       * phase.
       * - The applied arguments are compatible with AnyRef, which means
       *   that an argument tree typed as AnyVal has already been extended
       *   with the necessary boxing calls. This implies that passed
       *   arguments might not be strictly compatible with the method's
       *   parameter types (a boxed integer while int is expected).
       * - The expected return type is an AnyRef, even when the method's
       *   return type is an AnyVal. This means that the tree containing the
       *   call has already been extended with the necessary unboxing calls
       *   (or is happy with the boxed type).
       * - The type-checker has prevented dynamic applies on methods which
       *   parameter's erased types are not statically known at the call site.
       *   This is necessary to allow dispatching the call to the correct
       *   method (dispatching on parameters is static in Scala). In practice,
       *   this limitation only arises when the called method is defined as a
       *   refinement, where the refinement defines a parameter based on a
       *   type variable. */

      case tree: ApplyDynamic if tree.symbol.owner.isRefinementClass =>
        transformApplyDynamic(tree)

      /* Some cleanup transformations add members to templates (classes, traits, etc).
       * When inside a template (i.e. the body of one of its members), two maps
       * (newStaticMembers and newStaticInits) are available in the tree transformer. Any mapping from
       * a symbol to a MemberDef (DefDef, ValDef, etc.) that is in newStaticMembers once the
       * transformation of the template is finished will be added as a member to the
       * template. Any mapping from a symbol to a tree that is in newStaticInits, will be added
       * as a statement of the form "symbol = tree" to the beginning of the default
       * constructor. */
      case Template(parents, self, body) =>
        localTyper = typer.atOwner(tree, currentClass)
        transformTemplate(tree)

      case Literal(c) if c.tag == ClazzTag =>
        val tpe = c.typeValue
        typedWithPos(tree.pos) {
          if (isPrimitiveValueClass(tpe.typeSymbol)) {
            if (tpe.typeSymbol == UnitClass)
              REF(BoxedUnit_TYPE)
            else
              Select(REF(boxedModule(tpe.typeSymbol)), nme.TYPE_)
          }

          else tree
        }

     /*
      * This transformation should identify Scala symbol invocations in the tree and replace them
      * with references to a statically cached instance.
      *
      * The reasoning behind this transformation is the following. Symbols get interned - they are stored
      * in a global map which is protected with a lock. The reason for this is making equality checks
      * quicker. But calling Symbol.apply, although it does return a unique symbol, accesses a locked object,
      * making symbol access slow. To solve this, the unique symbol from the global symbol map in Symbol
      * is accessed only once during class loading, and after that, the unique symbol is in the statically
      * initialized call site returned by invokedynamic. Hence, it is cheap to both reach the unique symbol
      * and do equality checks on it.
      *
      * And, finally, be advised - Scala's Symbol literal (scala.Symbol) and the Symbol class of the compiler
      * have little in common.
      */
      case Apply(fn @ Select(qual, _), (arg @ Literal(Constant(symname: String))) :: Nil)
        if treeInfo.isQualifierSafeToElide(qual) && fn.symbol == Symbol_apply && !currentClass.isTrait =>

        super.transform(treeCopy.ApplyDynamic(tree, atPos(fn.pos)(Ident(SymbolLiteral_dummy).setType(SymbolLiteral_dummy.info)), LIT(SymbolLiteral_bootstrap) :: arg :: Nil))

      // Drop the TypeApply, which was used in Erasure to make `synchronized { ... } ` erase like `...`
      // (and to avoid boxing the argument to the polymorphic `synchronized` method).
      case app@Apply(TypeApply(fun, _), args) if fun.symbol == Object_synchronized =>
        super.transform(treeCopy.Apply(app, fun, args))

      // Replaces `Array(Predef.wrapArray(ArrayValue(...).$asInstanceOf[...]), <tag>)`
      // with just `ArrayValue(...).$asInstanceOf[...]`
      //
      // See SI-6611; we must *only* do this for literal vararg arrays.
      case Apply(appMeth, List(Apply(wrapRefArrayMeth, List(arg @ StripCast(ArrayValue(_, _)))), _))
      if wrapRefArrayMeth.symbol == currentRun.runDefinitions.Predef_wrapRefArray && appMeth.symbol == ArrayModule_genericApply =>
        super.transform(arg)
      case Apply(appMeth, List(elem0, Apply(wrapArrayMeth, List(rest @ ArrayValue(elemtpt, _)))))
      if wrapArrayMeth.symbol == Predef_wrapArray(elemtpt.tpe) && appMeth.symbol == ArrayModule_apply(elemtpt.tpe) =>
        super.transform(treeCopy.ArrayValue(rest, rest.elemtpt, elem0 :: rest.elems))

      case _ =>
        super.transform(tree)
    }

  } // CleanUpTransformer

}
