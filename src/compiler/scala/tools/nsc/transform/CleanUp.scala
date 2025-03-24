/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package transform

import symtab._
import Flags._
import scala.collection.mutable, mutable.{Buffer, ListBuffer}
import scala.tools.nsc.Reporting.WarningCategory
import scala.util.chaining._

abstract class CleanUp extends Statics with Transform with ast.TreeDSL {
  import global._
  import definitions._
  import CODE._
  import treeInfo.{ SYNTH_CASE_FLAGS, isDefaultCase, StripCast }

  val phaseName: String = "cleanup"

  /* used in GenBCode: collects ClassDef symbols owning a main(Array[String]) method */
  private val entryPoints = perRunCaches.newSet[Symbol]() // : List[Symbol] = Nil
  def getEntryPoints: List[String] = entryPoints.toList.map(_.fullName('.')).sorted

  protected def newTransformer(unit: CompilationUnit): AstTransformer =
    new CleanUpTransformer(unit)

  class CleanUpTransformer(unit: CompilationUnit) extends StaticsTransformer {
    private val newStaticMembers      = Buffer.empty[Tree]
    private val newStaticInits        = Buffer.empty[Tree]
    private val symbolsStoredAsStatic = mutable.Map.empty[String, Symbol]
    private var transformListApplyLimit = 8
    private def reducingTransformListApply[A](depth: Int)(body: => A): A = {
      val saved = transformListApplyLimit
      transformListApplyLimit -= depth
      try body
      finally transformListApplyLimit = saved
    }
    private def clearStatics(): Unit = {
      newStaticMembers.clear()
      newStaticInits.clear()
      symbolsStoredAsStatic.clear()
    }
    private def transformTemplate(tree: Tree) = {
      val Template(_, _, body) = tree: @unchecked
      clearStatics()
      val newBody = transformTrees(body)
      val templ   = deriveTemplate(tree)(_ => transformTrees(newStaticMembers.toList) ::: newBody)
      try
        if (newStaticInits.isEmpty) templ
        else deriveTemplate(templ)(body => staticConstructor(body, localTyper, templ.pos)(newStaticInits.toList) :: body)
      finally clearStatics()
    }
    private def mkTerm(prefix: String): TermName = unit.freshTermName(prefix)

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
        if (settings.logReflectiveCalls.value)
          reporter.echo(ad.pos, "method invocation uses reflection")

        val typedPos = typedWithPos(ad.pos) _

        assert(ad.symbol.isPublic, "Must be public")
        var qual: Tree = qual0

        /* ### CREATING THE METHOD CACHE ### */

        def addStaticMethodToClass(forBody: (Symbol, Symbol) => Tree): Symbol = {
          val methSym = currentClass.newMethod(mkTerm(nme.reflMethodName.toString), ad.pos, STATIC | SYNTHETIC)
          val params  = methSym.newSyntheticValueParams(List(ClassClass.tpe))
          methSym setInfoAndEnter MethodType(params, MethodClass.tpe)

          val methDef = typedPos(DefDef(methSym, forBody(methSym, params.head)))
          newStaticMembers += transform(methDef)
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

          gen.evalOnce(qual, currentOwner, localTyper.fresh) { qual1 =>
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
                case List(tp1, _) => (tp1 <:< IntTpe) && isMaybeUnit
                case _            => false
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
              def lookup      = Apply(cache, List(qual1().GETCLASS()))                                // get Method object from cache
              def invokeArgs  = ArrayValue(TypeTree(ObjectTpe), params)                       // args for invocation
              def invocation  = (lookup DOT invokeName)(qual1(), invokeArgs)                        // .invoke(qual1, ...)

              // exception catching machinery
              val invokeExc   = currentOwner.newValue(mkTerm(""), ad.pos) setInfo InvocationTargetExceptionClass.tpe
              def catchVar    = Bind(invokeExc, Typed(Ident(nme.WILDCARD), TypeTree(InvocationTargetExceptionClass.tpe)))
              def catchBody   = Throw(Apply(Select(Ident(invokeExc), nme.getCause), Nil))

              // try { method.invoke } catch { case e: InvocationTargetExceptionClass => throw e.getCause() }
              fixResult(TRY (invocation) CATCH { CASE (catchVar) ==> catchBody } FINALLY END)
            }

            /* A possible primitive method call, represented by methods in BoxesRunTime. */
            def genValueCall(operator: Symbol) = fixResult(REF(operator) APPLY args)
            def genValueCallWithTest =
              getPrimitiveReplacementForStructuralCall(methSym.name) match {
                case Some((operator, test)) =>
                  IF (test(qual1())) THEN genValueCall(operator) ELSE genDefaultCall
                case _ =>
                  genDefaultCall
              }

            /* A native Array call. */
            def genArrayCall = fixResult(
              methSym.name match {
                case nme.length => REF(boxMethod(IntClass)) APPLY (REF(arrayLengthMethod) APPLY args)
                case nme.update => REF(arrayUpdateMethod) APPLY List(args(0), (REF(unboxMethod(IntClass)) APPLY args(1)), args(2))
                case nme.apply  => REF(arrayApplyMethod) APPLY List(args(0), (REF(unboxMethod(IntClass)) APPLY args(1)))
                case nme.clone_ => REF(arrayCloneMethod) APPLY List(args(0))
                case x          => throw new MatchError(x)
              },
              mustBeUnit = methSym.name == nme.update
            )

            /* A conditional Array call, when we can't determine statically if the argument is
             * an Array, but the structural type method signature is consistent with an Array method
             * so we have to generate both kinds of code.
             */
            def genArrayCallWithTest =
              IF ((qual1().GETCLASS()) DOT nme.isArray) THEN genArrayCall ELSE genDefaultCall

            localTyper.typed {
              if (isMaybeBoxed && isJavaValueMethod) genValueCallWithTest
              else if (isArrayMethodSignature && isDefinitelyArray) genArrayCall
              else if (isArrayMethodSignature && isMaybeArray) genArrayCallWithTest
              else genDefaultCall
            }
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
              runReporting.warning(ad.pos,
                s"Overloaded type reached the backend! This is a bug in scalac.\n     Symbol: ${ad.symbol}\n  Overloads: $tpe\n  Arguments: " + ad.args.map(_.tpe),
                WarningCategory.Other,
                currentOwner)
              val fittingAlts = alts collect { case alt if sumSize(alt.paramss, 0) == params.length => alt.tpe }
              fittingAlts match {
                case mt @ MethodType(mparams, resType) :: Nil =>
                  runReporting.warning(ad.pos,
                    "Only one overload has the right arity, proceeding with overload " + mt,
                    WarningCategory.Other,
                    currentOwner)
                  (mparams, resType)
                case _ =>
                  reporter.error(ad.pos, "Cannot resolve overload.")
                  (Nil, NoType)
              }
            case NoType =>
              abort(ad.symbol.toString)
            case x => throw new MatchError(x)
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
        if (settings.isDebug) {
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

    object StringsPattern {
      def unapply(arg: Tree): Option[List[String]] = arg match {
        case Literal(Constant(value: String)) => Some(value :: Nil)
        case Literal(Constant(null))          => Some(null :: Nil)
        case Alternative(alts)                => traverseOpt(alts)(unapply).map(_.flatten)
        case _                                => None
      }
    }

    private def transformStringSwitch(sw: Match): Tree = { import CODE._
      // tree shape assumption justified by the codegen in MatchOptimization
      val Match(Typed(selTree0, _), cases) = sw: @unchecked
      // usually `selTree0` is an `Ident` or `Literal`, but Xasync may transform the scrutinee local into a state
      // machine field (scala/bug#12686). `evalOnce` introduces another local if needed (not for Ident / Literal).
      gen.evalOnce(selTree0, currentOwner, unit) { selTree =>
        def selArg = selTree() match {
          case x: Ident => REF(x.symbol)
          case x: Literal => x
          case x => throw new MatchError(x)
        }

        val newSel = selTree() match {
          case x: Ident => atPos(x.symbol.pos)(IF(x.symbol OBJ_EQ NULL) THEN ZERO ELSE selArg.OBJ_##)
          case x: Literal => atPos(x.pos)(if (x.value.value == null) ZERO else selArg.OBJ_##)
          case x => throw new MatchError(x)
        }
        val restpe = sw.tpe
        val resUnit = restpe =:= UnitTpe
        val swPos = sw.pos.focus

        /* From this:
         *     string match { case "AaAa" => 1 case "BBBB" | "c" => 2 case _ => 3 }
         * Generate this:
         *     string.## match {
         *       case 2031744 =>
         *              if ("AaAa" equals string) goto matchEnd (1)
         *         else if ("BBBB" equals string) goto case2
         *         else                           goto defaultCase
         *       case 99 =>
         *         if ("c" equals string) goto case2
         *         else                   goto defaultCase
         *       case _ => goto defaultCase
         *     }
         *     case2: goto matchEnd (2)
         *     defaultCase: goto matchEnd (3) // or `goto matchEnd (throw new MatchError(string))` if no default was given
         *     matchEnd(res: Int): res
         * Extra labels are added for alternative patterns branches, since multiple branches in the
         * resulting switch may need to correspond to a single case body.
         */

        val labels = ListBuffer.empty[LabelDef]
        var defaultCaseBody = Throw(New(MatchErrorClass.tpe_*, selArg)): Tree

        def LABEL(name: String) = currentOwner.newLabel(unit.freshTermName(name), swPos).setFlag(SYNTH_CASE_FLAGS)

        def newCase() = LABEL("case").setInfo(MethodType(Nil, restpe))

        val defaultCase = LABEL("defaultCase").setInfo(MethodType(Nil, restpe))
        val matchEnd = LABEL("matchEnd").tap { lab =>
          // genbcode isn't thrilled about seeing labels with Unit arguments, so `success`'s type is one of
          // `${sw.tpe} => ${sw.tpe}` or `() => Unit` depending.
          lab.setInfo(MethodType(if (resUnit) Nil else List(lab.newSyntheticValueParam(restpe)), restpe))
        }

        def goto(sym: Symbol, params: Tree*) = REF(sym) APPLY (params: _*)

        def gotoEnd(body: Tree) = if (resUnit) BLOCK(body, goto(matchEnd)) else goto(matchEnd, body)

        val casesByHash = cases.flatMap {
          case cd@CaseDef(StringsPattern(strs), _, body) =>
            val jump = newCase() // always create a label so when its used it matches the source case (e.g. `case4()`)
            strs match {
              case str :: Nil => List((str, gotoEnd(body), cd.pat.pos))
              case _ =>
                labels += LabelDef(jump, Nil, gotoEnd(body))
                strs.map((_, goto(jump), cd.pat.pos))
            }
          case cd if isDefaultCase(cd) => defaultCaseBody = gotoEnd(cd.body); None
          case cd => globalError(s"unhandled in switch: $cd"); None
        }.groupBy(_._1.##)

        val newCases = casesByHash.toList.sortBy(_._1).map {
          case (hash, cases) =>
            val newBody = cases.foldRight(atPos(swPos)(goto(defaultCase): Tree)) {
              case ((null, rhs, pos), next) => atPos(pos)(IF(NULL OBJ_EQ selArg) THEN rhs ELSE next)
              case ((str, rhs, pos), next) => atPos(pos)(IF(LIT(str) OBJ_== selArg) THEN rhs ELSE next)
            }
            CASE(LIT(hash)) ==> newBody
        }

        labels += LabelDef(defaultCase, Nil, defaultCaseBody)
        labels += LabelDef(matchEnd, matchEnd.info.params, matchEnd.info.params.headOption.fold(UNIT: Tree)(REF))

        val stats = Match(newSel, newCases :+ (DEFAULT ==> goto(defaultCase))) :: labels.toList

        val res = Block(stats: _*)
        typedWithPos(sw.pos)(res)
      }
    }

    // transform scrutinee of all matches to switchable types (ints, strings)
    def transformSwitch(sw: Match): Tree = {
      sw.selector.tpe.widen match {
        case IntTpe    => sw // can switch directly on ints
        case StringTpe => transformStringSwitch(sw)
        case _         => globalError(s"unhandled switch scrutinee type ${sw.selector.tpe}: $sw"); sw
      }
    }

    def transformApply(tree: Apply, fun: Tree, args: List[Tree]): Tree = tree match {
      case Apply(Select(qual, nm), Nil) if nm == nme.Nil && qual.symbol == ScalaPackageObject =>
        typedWithPos(tree.pos)(gen.mkNil)

      // Drop the TypeApply, which was used in Erasure to make `synchronized { ... } ` erase like `...`
      // (and to avoid boxing the argument to the polymorphic `synchronized` method).
      case Apply(TypeApply(sync, _), _) if sync.symbol == Object_synchronized =>
        treeCopy.Apply(tree, sync, args).transform(this)

      // Replaces `Array(<ScalaRunTime>.wrapArray(ArrayValue(...).$asInstanceOf[...]), <tag>)`
      // with just `ArrayValue(...).$asInstanceOf[...]`
      //
      // See scala/bug#6611; we must *only* do this for literal vararg arrays.
      case Apply(appMeth @ Select(appMethQual, _), Apply(wrapRefArrayMeth, (arg @ StripCast(ArrayValue(elemtpt, elems))) :: Nil) :: classTagEvidence :: Nil)
      if (wrapRefArrayMeth.symbol == currentRun.runDefinitions.wrapVarargsRefArrayMethod || wrapRefArrayMeth.symbol == currentRun.runDefinitions.genericWrapVarargsRefArrayMethod) &&
        appMeth.symbol == ArrayModule_genericApply && treeInfo.isQualifierSafeToElide(appMethQual) &&
        !elemtpt.tpe.typeSymbol.isBottomClass && !elemtpt.tpe.typeSymbol.isPrimitiveValueClass /* can happen via specialization.*/
      =>
        classTagEvidence.attachments.get[analyzer.MacroExpansionAttachment] match {
          case Some(att) if att.expandee.symbol.name == nme.materializeClassTag && tree.isInstanceOf[ApplyToImplicitArgs] =>
            super.transform(arg)
          case _ =>
            typedWithPos(tree.pos) {
              gen.evalOnce(classTagEvidence, currentOwner, unit) { _ =>
                val arr = typedWithPos(tree.pos)(gen.mkMethodCall(classTagEvidence, definitions.ClassTagClass.info.decl(nme.newArray), Nil, Literal(Constant(elems.size)) :: Nil))
                gen.evalOnce(arr, currentOwner, unit) { arr =>
                  val stats = ListBuffer[Tree]()
                  foreachWithIndex(elems) { (elem, i) =>
                    stats += gen.mkMethodCall(gen.mkAttributedRef(definitions.ScalaRunTimeModule), currentRun.runDefinitions.arrayUpdateMethod,
                                              Nil, arr() :: Literal(Constant(i)) :: elem :: Nil)
                  }
                  super.transform(Block(stats.toList, arr()))
                }
              }
            }
        }

      case Apply(appMeth @ Select(appMethQual, _), elem0 :: Apply(wrapArrayMeth, (rest @ ArrayValue(elemtpt, _)) :: Nil) :: Nil)
      if wrapArrayMeth.symbol == wrapVarargsArrayMethod(elemtpt.tpe) && appMeth.symbol == ArrayModule_apply(elemtpt.tpe) && treeInfo.isQualifierSafeToElide(appMethQual) =>
        treeCopy.ArrayValue(rest, rest.elemtpt, elem0 :: rest.elems).transform(this)

      // See scala/bug#12201, should be rewrite as Primitive Array.
      // Match Array
      case Apply(appMeth @ Select(appMethQual, _), Apply(wrapRefArrayMeth, StripCast(ArrayValue(elemtpt, elems)) :: Nil) :: _ :: Nil) 
      if appMeth.symbol == ArrayModule_genericApply && treeInfo.isQualifierSafeToElide(appMethQual) && currentRun.runDefinitions.primitiveWrapArrayMethod.contains(wrapRefArrayMeth.symbol) =>
        typedWithPos(elemtpt.pos)(
          ArrayValue(TypeTree(elemtpt.tpe), elems)
        ).transform(this)

      case Apply(appMeth @ Select(appMethQual, _), elem :: (nil: RefTree) :: Nil)
      if nil.symbol == NilModule && appMeth.symbol == ArrayModule_apply(elem.tpe.widen) && treeInfo.isExprSafeToInline(nil) && treeInfo.isQualifierSafeToElide(appMethQual) =>
        typedWithPos(elem.pos)(
          ArrayValue(TypeTree(elem.tpe), elem :: Nil)
        ).transform(this)

      // <List or Seq>(a, b, c) ~> new ::(a, new ::(b, new ::(c, Nil))) but only for reference types
      case StripCast(Apply(appMeth @ Select(_, _), List(Apply(wrapArrayMeth, List(StripCast(rest @ ArrayValue(elemtpt, _)))))))
      if wrapArrayMeth.symbol == currentRun.runDefinitions.wrapVarargsRefArrayMethod
        && currentRun.runDefinitions.isSeqApply(appMeth)  // includes List
        && rest.elems.lengthIs < transformListApplyLimit
      =>
        val consed = rest.elems.foldRight(gen.mkAttributedRef(NilModule): Tree)(New(ConsClass, _, _))
        // Limiting extra stack frames consumed by generated code
        reducingTransformListApply(rest.elems.length) {
          super.transform(typedWithPos(tree.pos)(consed))
        }

      //methods on Double
      //new Predef.doubleToDouble(x).isNaN() -> java.lang.Double.isNaN(x)
      //new Predef.doubleToDouble(x).isInfinite() -> java.lang.Double.isInfinity(x)
      //methods on Float
      //new Predef.float2Float(x).isNaN() -> java.lang.Double.isNaN(x)
      //new Predef.float2Float(x).isInfinite() -> java.lang.Double.isInfinity(x)

      //methods on Number
      //new Predef.<convert>(x).byteValue() -> x.toByte()
      //new Predef.<convert>(x).shortValue() -> x.toShort()
      //new Predef.<convert>(x).intValue() -> x.toInt()
      //new Predef.<convert>(x).longValue() -> x.toLong()
      //new Predef.<convert>(x).floatValue() -> x.toFloat()
      //new Predef.<convert>(x).doubleValue() -> x.toDouble()
      //
      // for each of the conversions
      // double2Double
      // float2Float
      // byte2Byte
      // short2Short
      // char2Character
      // int2Integer
      // long2Long
      // boolean2Boolean
      //
      case Apply(Select(Apply(boxing @ Select(qual, _), params), methodName), Nil)
      if currentRun.runDefinitions.PreDef_primitives2Primitives.contains(boxing.symbol) &&
          params.size == 1 &&
          allPrimitiveMethodsToRewrite.contains(methodName) &&
          treeInfo.isExprSafeToInline(qual)
      =>
        val newTree =
          if (doubleAndFloatRedirectMethods.contains(methodName)) {
            val cls =
              if (boxing.symbol == currentRun.runDefinitions.Predef_double2Double)
                definitions.BoxedDoubleClass
              else definitions.BoxedFloatClass

            val targetMethod = cls.companionModule.info.decl(doubleAndFloatRedirectMethods(methodName))
            gen.mkMethodCall(targetMethod, params)
          } else {
            gen.mkMethodCall(Select(params.head, javaNumberConversions(methodName)), Nil)
          }
        super.transform(typedWithPos(tree.pos)(newTree))

      //(x:Int).hashCode is transformed to scala.Int.box(x).hashCode()
      //(x:Int).toString is transformed to scala.Int.box(x).toString()
      //
      //rewrite
      // scala.Int.box(x).hashCode() ->  java.lang.Integer.hashCode(x)
      // scala.Int.box(x).toString() ->  java.lang.Integer.toString(x)
      // similarly for all primitive types
      case Apply(Select(Apply(box @ Select(boxer, _), params), methodName), Nil)
      if objectMethods.contains(methodName) &&
          params.size == 1 &&
          currentRun.runDefinitions.isBox(box.symbol) &&
          treeInfo.isExprSafeToInline(boxer)
      =>
        val target = boxedClass(boxer.symbol.companion)
        val targetMethod = target.companionModule.info.decl(methodName)
        val newTree      = gen.mkMethodCall(targetMethod, params)
        super.transform(typedWithPos(tree.pos)(newTree))

      // Seq() ~> Nil (note: List() ~> Nil is rewritten in the Typer)
      case Apply(Select(_, _), List(nil))
      if currentRun.runDefinitions.isNil(nil.symbol) && currentRun.runDefinitions.isSeqApply(fun) =>
        gen.mkAttributedRef(NilModule)

      /* This transformation should identify Scala symbol invocations in the tree and replace them
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
      case Apply(Select(qual, _), (arg @ Literal(Constant(_: String))) :: Nil)
      if fun.symbol == Symbol_apply && !currentClass.isTrait && treeInfo.isQualifierSafeToElide(qual) =>

        treeCopy.ApplyDynamic(tree, atPos(fun.pos)(Ident(SymbolLiteral_dummy).setType(SymbolLiteral_dummy.info)), LIT(SymbolLiteral_bootstrap) :: arg :: Nil).transform(this)

      case _ =>
        super.transform(tree)
    }

    override def transform(tree: Tree): Tree = tree match {
      case _: ClassDef if genBCode.codeGen.CodeGenImpl.isJavaEntryPoint(tree.symbol, currentUnit, settings.mainClass.valueSetByUser.map(_.toString)) =>
        // collecting symbols for entry points here (as opposed to GenBCode where they are used)
        // has the advantage of saving an additional pass over all ClassDefs.
        entryPoints += tree.symbol
        tree.transform(this)

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

      case t @ Apply(fun, args) =>
        transformApply(t, fun, args)

      case switch: Match =>
        super.transform(transformSwitch(switch))

      case _ =>
        super.transform(tree)
    }

  } // end CleanUpTransformer


  private val objectMethods = Map[Name, TermName](
    nme.hashCode_ -> nme.hashCode_,
    nme.toString_ -> nme.toString_
    )
  private val doubleAndFloatRedirectMethods = Map[Name, TermName](
    nme.isNaN -> nme.isNaN,
    nme.isInfinite -> nme.isInfinite
    )
  private val javaNumberConversions         = Map[Name, TermName](
    nme.byteValue -> nme.toByte,
    nme.shortValue -> nme.toShort,
    nme.intValue -> nme.toInt,
    nme.longValue -> nme.toLong,
    nme.floatValue -> nme.toFloat,
    nme.doubleValue -> nme.toDouble
    )
  private val allPrimitiveMethodsToRewrite  = doubleAndFloatRedirectMethods.keySet ++ javaNumberConversions.keySet

}
