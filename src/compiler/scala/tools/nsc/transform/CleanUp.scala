/* NSC -- new Scala compiler
 * Copyrights 2005-2010 LAMP/EPFL
 * @author Martin Odersky
 */
// $Id$

package scala.tools.nsc
package transform

import symtab._
import Flags._
import scala.collection.mutable.{ListBuffer, HashMap}

abstract class CleanUp extends Transform with ast.TreeDSL {
  import global._
  import definitions._
  import CODE._

  /** the following two members override abstract members in Transform */
  val phaseName: String = "cleanup"

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new CleanUpTransformer(unit)

  class CleanUpTransformer(unit: CompilationUnit) extends Transformer {
    private val newDefs = new ListBuffer[Tree]
    private val newInits = new ListBuffer[Tree]

    private val classConstantMeth = new HashMap[String, Symbol]
    private val symbolStaticFields = new HashMap[String, (Symbol, Tree, Tree)]

    private var localTyper: analyzer.Typer = null

    private lazy val serializableAnnotation =
      AnnotationInfo(SerializableAttr.tpe, Nil, Nil)
    private lazy val serialVersionUIDAnnotation = {
      val attr = definitions.getClass("scala.SerialVersionUID")
      AnnotationInfo(attr.tpe, List(Literal(Constant(0))), List())
    }

    private object MethodDispatchType extends scala.Enumeration {
      val NO_CACHE, MONO_CACHE, POLY_CACHE = Value
    }
    import MethodDispatchType.{ NO_CACHE, MONO_CACHE, POLY_CACHE }
    private def dispatchType() = settings.refinementMethodDispatch.value match {
      case "no-cache"   => NO_CACHE
      case "mono-cache" => MONO_CACHE
      case "poly-cache" => POLY_CACHE
    }

    private def typedWithPos(pos: Position)(tree: Tree) =
      localTyper typed { atPos(pos)(tree) }

    override def transformUnit(unit: CompilationUnit) =
      unit.body = transform(unit.body)

    /** A value class is defined to be only Java-compatible values: unit is
      * not part of it, as opposed to isValueClass in definitions. scala.Int is
      * a value class, java.lang.Integer is not. */
    def isJavaValueClass(sym: Symbol) = boxedClass contains sym
    def isJavaValueType(tp: Type) = isJavaValueClass(tp.typeSymbol)

    /** The boxed type if it's a primitive; identity otherwise.
     */
    def toBoxedType(tp: Type) = if (isJavaValueType(tp)) boxedClass(tp.typeSymbol).tpe else tp

    override def transform(tree: Tree): Tree = tree match {

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
      case ad@ApplyDynamic(qual0, params) =>
        def mkName(s: String = "") =
          if (s == "") unit.fresh newName ad.pos
          else unit.fresh.newName(ad.pos, s)
        def mkTerm(s: String = "") = newTermName(mkName(s))
        val typedPos = typedWithPos(ad.pos) _

        assert(ad.symbol.isPublic)
        var qual: Tree = qual0

        /* ### CREATING THE METHOD CACHE ### */

        def addStaticVariableToClass(forName: String, forType: Type, forInit: Tree, isFinal: Boolean): Symbol = {
          val varSym = currentClass.newVariable(ad.pos, mkName(forName))
            .setFlag(PRIVATE | STATIC | MUTABLE | SYNTHETIC)
            .setInfo(forType)
          if (isFinal) varSym setFlag FINAL else varSym addAnnotation AnnotationInfo(VolatileAttr.tpe, Nil, Nil)
          currentClass.info.decls enter varSym

          val varDef = typedPos( VAL(varSym) === forInit )
          newDefs append transform(varDef)

          val varInit = typedPos( REF(varSym) === forInit )
          newInits append transform(varInit)

          varSym
        }

        def addStaticMethodToClass(forName: String, forArgsTypes: List[Type], forResultType: Type)
                                  (forBody: Pair[Symbol, List[Symbol]] => Tree): Symbol = {
          val methSym = currentClass.newMethod(ad.pos, mkName(forName))
            .setFlag(STATIC | SYNTHETIC)

          methSym.setInfo(MethodType(methSym.newSyntheticValueParams(forArgsTypes), forResultType))
          currentClass.info.decls enter methSym

          val methDef = typedPos( DefDef(methSym, { forBody(Pair(methSym, methSym.paramss(0))) }) )
          newDefs append transform(methDef)

          methSym
        }

        def fromTypesToClassArrayLiteral(paramTypes: List[Type]): Tree =
          ArrayValue(TypeTree(ClassClass.tpe), paramTypes map LIT)

        def theTypeClassArray =
          TypeRef(ArrayClass.tpe.prefix, ArrayClass, List(ClassClass.tpe))

        /* ... */
        def reflectiveMethodCache(method: String, paramTypes: List[Type]): Symbol = dispatchType match {
          case NO_CACHE =>

              /* Implementation of the cache is as follows for method "def xyz(a: A, b: B)":

                var reflParams$Cache: Array[Class[_]] = Array[JClass](classOf[A], classOf[B])

                def reflMethod$Method(forReceiver: JClass[_]): JMethod =
                  forReceiver.getMethod("xyz", reflParams$Cache)

              */

              val reflParamsCacheSym: Symbol =
                addStaticVariableToClass("reflParams$Cache", theTypeClassArray, fromTypesToClassArrayLiteral(paramTypes), true)

              addStaticMethodToClass("reflMethod$Method", List(ClassClass.tpe), MethodClass.tpe) {
                case Pair(reflMethodSym, List(forReceiverSym)) =>
                  (REF(forReceiverSym) DOT Class_getMethod)(LIT(method), REF(reflParamsCacheSym))
              }

            case MONO_CACHE =>

              /* Implementation of the cache is as follows for method "def xyz(a: A, b: B)"
                 (but with a SoftReference wrapping reflClass$Cache, similarly in the poly Cache) :

                var reflParams$Cache: Array[Class[_]] = Array[JClass](classOf[A], classOf[B])

                var reflMethod$Cache: JMethod = null

                var reflClass$Cache: JClass[_] = null

                def reflMethod$Method(forReceiver: JClass[_]): JMethod = {
                  if (reflClass$Cache != forReceiver) {
                    reflMethod$Cache = forReceiver.getMethod("xyz", reflParams$Cache)
                    reflClass$Cache = forReceiver
                  }
                  reflMethod$Cache
                }

              */

              val reflParamsCacheSym: Symbol =
                addStaticVariableToClass("reflParams$Cache", theTypeClassArray, fromTypesToClassArrayLiteral(paramTypes), true)

              val reflMethodCacheSym: Symbol =
                addStaticVariableToClass("reflMethod$Cache", MethodClass.tpe, NULL, false)

              val reflClassCacheSym: Symbol =
                addStaticVariableToClass("reflClass$Cache", SoftReferenceClass.tpe, NULL, false)

              def getMethodSym = ClassClass.tpe member nme.getMethod_

              def isCacheEmpty(receiver: Symbol): Tree =
                reflClassCacheSym.IS_NULL() OR (reflClassCacheSym.GET() ANY_NE REF(receiver))

              addStaticMethodToClass("reflMethod$Method", List(ClassClass.tpe), MethodClass.tpe) {
                case Pair(reflMethodSym, List(forReceiverSym)) =>
                  BLOCK(
                    IF (isCacheEmpty(forReceiverSym)) THEN BLOCK(
                      REF(reflMethodCacheSym) === ((REF(forReceiverSym) DOT getMethodSym)(LIT(method), REF(reflParamsCacheSym))) ,
                      REF(reflClassCacheSym) === gen.mkSoftRef(REF(forReceiverSym)),
                      UNIT
                    ) ENDIF,
                    REF(reflMethodCacheSym)
                  )
              }

            case POLY_CACHE =>

              /* Implementation of the cache is as follows for method "def xyz(a: A, b: B)"
                 (but with the addition of a SoftReference wrapped around the MethodCache holder
                 so that it does not interfere with classloader garbage collection, see ticket
                 #2365 for details):

                var reflParams$Cache: Array[Class[_]] = Array[JClass](classOf[A], classOf[B])

                var reflPoly$Cache: scala.runtime.MethodCache = new EmptyMethodCache()

                def reflMethod$Method(forReceiver: JClass[_]): JMethod = {
                  var method: JMethod = reflPoly$Cache.find(forReceiver)
                  if (method != null)
                    return method
                  else {
                    method = forReceiver.getMethod("xyz", reflParams$Cache)
                    reflPoly$Cache = reflPoly$Cache.add(forReceiver, method)
                    return method
                  }
                }

              */

              val reflParamsCacheSym: Symbol =
                addStaticVariableToClass("reflParams$Cache", theTypeClassArray, fromTypesToClassArrayLiteral(paramTypes), true)

              def mkNewPolyCache = gen.mkSoftRef(NEW(TypeTree(EmptyMethodCacheClass.tpe)))
              val reflPolyCacheSym: Symbol = addStaticVariableToClass("reflPoly$Cache", SoftReferenceClass.tpe, mkNewPolyCache, false)
              def getPolyCache = fn(REF(reflPolyCacheSym), nme.get) AS_ATTR MethodCacheClass.tpe

              addStaticMethodToClass("reflMethod$Method", List(ClassClass.tpe), MethodClass.tpe)
                { case Pair(reflMethodSym, List(forReceiverSym)) =>
                  val methodSym = reflMethodSym.newVariable(ad.pos, mkTerm("method")) setInfo MethodClass.tpe

                  BLOCK(
                    IF (getPolyCache ANY_EQ NULL) THEN (REF(reflPolyCacheSym) === mkNewPolyCache) ENDIF,
                    VAL(methodSym) === ((getPolyCache DOT methodCache_find)(REF(forReceiverSym))) ,
                    IF (REF(methodSym) OBJ_!= NULL) .
                      THEN (Return(REF(methodSym)))
                    ELSE {
                      def methodSymRHS  = ((REF(forReceiverSym) DOT Class_getMethod)(LIT(method), REF(reflParamsCacheSym)))
                      def cacheRHS      = ((getPolyCache DOT methodCache_add)(REF(forReceiverSym), REF(methodSym)))
                      BLOCK(
                        REF(methodSym)        === methodSymRHS,
                        REF(reflPolyCacheSym) === gen.mkSoftRef(cacheRHS),
                        Return(REF(methodSym))
                      )
                    }
                  )
                }
        }

        /* ### HANDLING METHODS NORMALLY COMPILED TO OPERATORS ### */

        val testForNumber: Tree     = (qual IS_OBJ BoxedNumberClass.tpe) OR (qual IS_OBJ BoxedCharacterClass.tpe)
        val testForBoolean: Tree    = (qual IS_OBJ BoxedBooleanClass.tpe)
        val testForNumberOrBoolean  = testForNumber OR testForBoolean

        val getPrimitiveReplacementForStructuralCall: PartialFunction[Name, (Symbol, Tree)] = {
          val testsForNumber = Map() ++ List(
            nme.UNARY_+ -> "positive",
            nme.UNARY_- -> "negate",
            nme.UNARY_~ -> "complement",
            nme.ADD     -> "add",
            nme.SUB     -> "subtract",
            nme.MUL     -> "multiply",
            nme.DIV     -> "divide",
            nme.MOD     -> "takeModulo",
            nme.LSL     -> "shiftSignedLeft",
            nme.LSR     -> "shiftLogicalRight",
            nme.ASR     -> "shiftSignedRight",
            nme.LT      -> "testLessThan",
            nme.LE      -> "testLessOrEqualThan",
            nme.GE      -> "testGreaterOrEqualThan",
            nme.GT      -> "testGreaterThan",
            nme.toByte  -> "toByte",
            nme.toShort -> "toShort",
            nme.toChar  -> "toCharacter",
            nme.toInt   -> "toInteger",
            nme.toLong  -> "toLong",
            nme.toFloat -> "toFloat",
            nme.toDouble-> "toDouble"
          )
          val testsForBoolean = Map() ++ List(
            nme.UNARY_! -> "takeNot",
            nme.ZOR     -> "takeConditionalOr",
            nme.ZAND    -> "takeConditionalAnd"
          )
          val testsForNumberOrBoolean = Map() ++ List(
            nme.OR      -> "takeOr",
            nme.XOR     -> "takeXor",
            nme.AND     -> "takeAnd",
            nme.EQ      -> "testEqual",
            nme.NE      -> "testNotEqual"
          )
          def get(name: String) = getMember(BoxesRunTimeClass, name)

          /** Begin partial function. */
          {
            case x if testsForNumber contains x           => (get(testsForNumber(x)), testForNumber)
            case x if testsForBoolean contains x          => (get(testsForBoolean(x)), testForBoolean)
            case x if testsForNumberOrBoolean contains x  => (get(testsForNumberOrBoolean(x)), testForNumberOrBoolean)
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
          /* Some info about the type of the method being called. */
          val methSym       = ad.symbol
          val boxedResType  = toBoxedType(resType)      // Int -> Integer
          val resultSym     = boxedResType.typeSymbol
          // If this is a primitive method type (like '+' in 5+5=10) then the
          // parameter types and the (unboxed) result type should all be primitive types,
          // and the method name should be in the primitive->structural map.
          def isJavaValueMethod = (
            (resType :: paramTypes forall isJavaValueType) && // issue #1110
            (getPrimitiveReplacementForStructuralCall isDefinedAt methSym.name)
          )
          // Erasure lets Unit through as Unit, but a method returning Any will have an
          // erased return type of Object and should also allow Unit.
          def isDefinitelyUnit  = (resultSym == UnitClass)
          def isMaybeUnit       = (resultSym == ObjectClass) || isDefinitelyUnit
          // If there's any chance this signature could be met by an Array.
          val isArrayMethodSignature = {
            def typesMatchApply = paramTypes match {
              case List(tp) => tp <:< IntClass.tpe
              case _        => false
            }
            def typesMatchUpdate = paramTypes match {
              case List(tp1, tp2) => (tp1 <:< IntClass.tpe) && isMaybeUnit
              case _              => false
            }

            (methSym.name == nme.length && params.isEmpty) ||
            (methSym.name == nme.clone_ && params.isEmpty) ||
            (methSym.name == nme.apply  && typesMatchApply) ||
            (methSym.name == nme.update && typesMatchUpdate)
          }

          /* Some info about the argument at the call site. */
          val qualSym           = qual.tpe.typeSymbol
          val args              = qual :: params
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
            else tree AS_ATTR boxedResType                                              // cast to expected type

          /** Normal non-Array call */
          def genDefaultCall = {
            // reflective method call machinery
            val invokeName  = MethodClass.tpe member nme.invoke_                                // reflect.Method.invoke(...)
            def cache       = REF(reflectiveMethodCache(ad.symbol.name.toString, paramTypes))   // cache Symbol
            def lookup      = Apply(cache, List(qual GETCLASS))                                 // get Method object from cache
            def invokeArgs  = ArrayValue(TypeTree(ObjectClass.tpe), params)                     // args for invocation
            def invocation  = (lookup DOT invokeName)(qual, invokeArgs)                         // .invoke(qual, ...)

            // exception catching machinery
            val invokeExc   = currentOwner.newValue(ad.pos, mkTerm()) setInfo InvocationTargetExceptionClass.tpe
            def catchVar    = Bind(invokeExc, Typed(Ident(nme.WILDCARD), TypeTree(InvocationTargetExceptionClass.tpe)))
            def catchBody   = Throw(Apply(Select(Ident(invokeExc), nme.getCause), Nil))

            // try { method.invoke } catch { case e: InvocationTargetExceptionClass => throw e.getCause() }
            fixResult(TRY (invocation) CATCH { CASE (catchVar) ==> catchBody } ENDTRY)
          }

          /** A possible primitive method call, represented by methods in BoxesRunTime. */
          def genValueCall(operator: Symbol) = fixResult(REF(operator) APPLY args)
          def genValueCallWithTest = {
            val (operator, test)  = getPrimitiveReplacementForStructuralCall(methSym.name)
            IF (test) THEN genValueCall(operator) ELSE genDefaultCall
          }

          /** A native Array call. */
          def genArrayCall = fixResult(
            methSym.name match {
              case nme.length => REF(boxMethod(IntClass)) APPLY (REF(arrayLengthMethod) APPLY args)
              case nme.update => REF(arrayUpdateMethod) APPLY List(args(0), (REF(unboxMethod(IntClass)) APPLY args(1)), args(2))
              case nme.apply  => REF(arrayApplyMethod) APPLY List(args(0), (REF(unboxMethod(IntClass)) APPLY args(1)))
              case nme.clone_ => REF(arrayCloneMethod) APPLY List(args(0))
            },
            mustBeUnit = methSym.name == nme.update
          )

          /** A conditional Array call, when we can't determine statically if the argument is
           *  an Array, but the structural type method signature is consistent with an Array method
           *  so we have to generate both kinds of code.
           */
          def genArrayCallWithTest =
            IF ((qual GETCLASS()) DOT nme.isArray) THEN genArrayCall ELSE genDefaultCall

          localTyper typed (
            if (isMaybeBoxed && isJavaValueMethod) genValueCallWithTest
            else if (isArrayMethodSignature && isDefinitelyArray) genArrayCall
            else if (isArrayMethodSignature && isMaybeArray) genArrayCallWithTest
            else genDefaultCall
          )
        }

        if (settings.refinementMethodDispatch.value == "invoke-dynamic") {
/*          val guardCallSite: Tree = {
            val cachedClass = addStaticVariableToClass("cachedClass", definitions.ClassClass.tpe, EmptyTree)
            val tmpVar = currentOwner.newVariable(ad.pos, unit.fresh.newName(ad.pos, "x")).setInfo(definitions.AnyRefClass.tpe)
            atPos(ad.pos)(Block(List(
              ValDef(tmpVar, transform(qual))),
              If(Apply(Select(gen.mkAttributedRef(cachedClass), nme.EQ), List(getClass(Ident(tmpVar)))),
                 Block(List(Assign(gen.mkAttributedRef(cachedClass), getClass(Ident(tmpVar)))),
                       treeCopy.ApplyDynamic(ad, Ident(tmpVar), transformTrees(params))),
                 EmptyTree)))
          }
          //println(guardCallSite)
*/
          localTyper.typed(treeCopy.ApplyDynamic(ad, transform(qual), transformTrees(params)))
        }
        else {

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
          val t: Tree = ad.symbol.tpe match {
            case MethodType(mparams, resType) =>
              assert(params.length == mparams.length)
              typedPos {
                val sym = currentOwner.newValue(ad.pos, mkTerm("qual")) setInfo qual0.tpe
                qual = REF(sym)

                BLOCK(
                  VAL(sym) === qual0,
                  callAsReflective(mparams map (_.tpe), resType)
                )
              }
          }

          /* For testing purposes, the dynamic application's condition
           * can be printed-out in great detail. Remove? */
          if (settings.debug.value) {
            def paramsToString(xs: Any*) = xs map (_.toString) mkString ", "
            val mstr = ad.symbol.tpe match {
              case MethodType(mparams, resType) =>
                """|  with
                   |  - declared parameter types: '%s'
                   |  - passed argument types:    '%s'
                   |  - result type:              '%s'""" .
                  stripMargin.format(
                     paramsToString(mparams),
                     paramsToString(params),
                     resType.toString
                  )
              case _ => ""
            }
            Console.printf("""Dynamically application '%s.%s(%s)' %s - resulting code: '%s'""",
                List(qual, ad.symbol.name, paramsToString(params), mstr, t) map (_.toString) : _*
            )
          }

          /* We return the dynamic call tree, after making sure no other
           * clean-up transformation are to be applied on it. */
          transform(t)
        }
        /* ### END OF DYNAMIC APPLY TRANSFORM ### */

      /* Some cleanup transformations add members to templates (classes, traits, etc).
       * When inside a template (i.e. the body of one of its members), two maps
       * (newDefs and newInits) are available in the tree transformer. Any mapping from
       * a symbol to a MemberDef (DefDef, ValDef, etc.) that is in newDefs once the
       * transformation of the template is finished will be added as a member to the
       * template. Any mapping from a symbol to a tree that is in newInits, will be added
       * as a statement of the form "symbol = tree" to the beginning of the default
       * constructor. */
      case Template(parents, self, body) =>
        localTyper = typer.atOwner(tree, currentClass)
        val transformedTemplate = if (!forMSIL) {
          classConstantMeth.clear
          newDefs.clear
          newInits.clear
          var newBody =
            transformTrees(body)
          val firstConstructor =
            treeInfo.firstConstructor(newBody)
          newBody =
            transformTrees(newDefs.toList) ::: (
              for (member <- newBody) yield member match {
                case thePrimaryConstructor@DefDef(mods, name, tparams, vparamss, tpt, rhs) if (thePrimaryConstructor == firstConstructor) =>
                  val newRhs = rhs match {
                    case theRhs@Block(stats, expr) =>
                      treeCopy.Block(theRhs, transformTrees(newInits.toList) ::: stats, expr)
                  }
                  treeCopy.DefDef(thePrimaryConstructor, mods, name, tparams, vparamss, tpt, newRhs)
                case notThePrimaryConstructor =>
                  notThePrimaryConstructor
              }
            )
            treeCopy.Template(tree, parents, self, newBody)
        }
        else super.transform(tree)
        applySymbolFieldInitsToStaticCtor(transformedTemplate.asInstanceOf[Template]) // postprocess to include static ctors

      case Literal(c) if (c.tag == ClassTag) && !forMSIL=>
        val tpe = c.typeValue
        typedWithPos(tree.pos) {
          if (isValueClass(tpe.typeSymbol)) {
            if (tpe.typeSymbol == UnitClass)
              Select(REF(BoxedUnit_TYPE), BoxedUnit_TYPE)
            else
              Select(REF(boxedModule(tpe.typeSymbol)), nme.TYPE_)
          }

          else tree
        }

      /* MSIL requires that the stack is empty at the end of a try-block.
       * Hence, we here rewrite all try blocks with a result != {Unit, All} such that they
       * store their result in a local variable. The catch blocks are adjusted as well.
       * The try tree is subsituted by a block whose result expression is read of that variable. */
      case theTry @ Try(block, catches, finalizer)
        if theTry.tpe.typeSymbol != definitions.UnitClass && theTry.tpe.typeSymbol != definitions.NothingClass =>
        val tpe = theTry.tpe.widen
        val tempVar = currentOwner.newValue(theTry.pos, unit.fresh.newName(theTry.pos, "exceptionResult"))
          .setInfo(tpe).setFlag(Flags.MUTABLE)
        def assignBlock(rhs: Tree) = super.transform(BLOCK(Ident(tempVar) === transform(rhs)))

        val newBlock    = assignBlock(block)
        val newCatches  = for (CaseDef(pattern, guard, body) <- catches) yield
          (CASE(super.transform(pattern)) IF (super.transform(guard))) ==> assignBlock(body)
        val newTry      = Try(newBlock, newCatches, super.transform(finalizer))

        localTyper typed { BLOCK(VAL(tempVar) === EmptyTree, newTry, Ident(tempVar)) }

      /* Adds @serializable annotation to anonymous function classes */
      case cdef @ ClassDef(mods, name, tparams, impl) =>
        if (settings.target.value == "jvm-1.5") {
          val sym = cdef.symbol
          // is this an anonymous function class?
          if (sym.isAnonymousFunction && !sym.hasAnnotation(SerializableAttr)) {
            sym addAnnotation serializableAnnotation
            sym addAnnotation serialVersionUIDAnnotation
          }
        }
        super.transform(tree)

     /*
      * This transformation should identify Scala symbol invocations in the tree and replace them
      * with references to a static member. Also, whenever a class has at least a single symbol invocation
      * somewhere in its methods, a new static member should be created and initialized for that symbol.
      * For instance, say we have a Scala class:
      *
      * class Cls {
      *   // ...
      *   def someSymbol = `symbolic
      *   // ...
      * }
      *
      * After transformation, this class looks like this:
      *
      * class Cls {
      *   private "static" val <some_name>$symbolic = Symbol("symbolic")
      *   // ...
      *   def someSymbol = <some_name>$symbolic
      *   // ...
      * }
      *
      * The reasoning behind this transformation is the following. Symbols get interned - they are stored
      * in a global map which is protected with a lock. The reason for this is making equality checks
      * quicker. But calling Symbol.apply, although it does return a unique symbol, accesses a locked object,
      * making symbol access slow. To solve this, the unique symbol from the global symbol map in Symbol
      * is accessed only once during class loading, and after that, the unique symbol is in the static
      * member. Hence, it is cheap to both reach the unique symbol and do equality checks on it.
      *
      * And, finally, be advised - scala symbol literal and the Symbol class of the compiler
      * have little in common.
      */
      case symapp @ Apply(Select(Select(a @ Ident(nme.scala_), b @ nme.Symbol), nme.apply),
                          List(Literal(Constant(symname: String)))) =>
        // add the symbol name to a map if it's not there already
        val rhs = gen.mkCast(Apply(gen.scalaDot(nme.Symbol), List(Literal(Constant(symname)))), symbolType)
        val (staticFieldSym, sfdef, sfinit) = getSymbolStaticField(symapp.pos, symname, rhs, symapp)

        // create a reference to a static field
        val ntree = typedWithPos(symapp.pos)(REF(staticFieldSym))

        super.transform(ntree)
      case _ =>
        super.transform(tree)
    }

    /* Returns the symbol and the tree for the symbol field interning a reference to a symbol 'synmname'.
     * If it doesn't exist, i.e. the symbol is encountered the first time,
     * it creates a new static field definition and initialization and returns it.
     */
    private def getSymbolStaticField(pos: Position, symname: String, rhs: Tree, tree: Tree): (Symbol, Tree, Tree) =
      symbolStaticFields.getOrElseUpdate(symname, {
        val freshname = unit.fresh.newName(pos, "symbol$")
        val theTyper = typer.atOwner(tree, currentClass)

        // create a symbol for the static field
        val stfieldSym = currentClass.newVariable(pos, freshname)
          .setFlag(PRIVATE | STATIC | SYNTHETIC | FINAL)
          .setInfo(symbolType)
        currentClass.info.decls enter stfieldSym

        // create field definition and initialization
        val stfieldDef = theTyper.typed { atPos(pos)(VAL(stfieldSym) === rhs) }
        val stfieldInit = theTyper.typed { atPos(pos)(REF(stfieldSym) === rhs) }

        // add field definition to new defs
        newDefs append stfieldDef

        (stfieldSym, stfieldDef, stfieldInit)
      })

    /* returns a list of all trees for symbol static fields, and clear the list */
    private def flushSymbolFieldsInitializations: List[Tree] = {
      val fields = (symbolStaticFields.valuesIterator map (_._3)).toList
      symbolStaticFields.clear
      fields
    }

    /* finds the static ctor DefDef tree within the template if it exists. */
    def findStaticCtor(template: Template): Option[Tree] =
      template.body find {
        case defdef @ DefDef(mods, nme.CONSTRUCTOR, tparam, vparam, tp, rhs) => defdef.symbol hasFlag STATIC
        case _ => false
      }

    /* changes the template for the class so that it contains a static constructor with symbol fields inits,
     * augments an existing static ctor if one already existed.
     */
    def applySymbolFieldInitsToStaticCtor(template: Template): Template = {
      val symbolInitTrees = flushSymbolFieldsInitializations
      if (symbolInitTrees.isEmpty) template
      else {
        val theTyper = typer.atOwner(template, currentClass)
        val newCtor = findStaticCtor(template) match {
          // in case there already were static ctors - augment existing ones
          // currently, however, static ctors aren't being generated anywhere else
          case Some(ctor @ DefDef(mods, name, tparams, vparamss, tpt, rhs)) =>
            // modify existing static ctor
            val newBlock = rhs match {
              case block @ Block(stats, expr) =>
                // need to add inits to existing block
                treeCopy.Block(block, symbolInitTrees ::: stats, expr)
              case term: TermTree =>
                // need to create a new block with inits and the old term
                treeCopy.Block(term, symbolInitTrees, term)
            }
            treeCopy.DefDef(ctor, mods, name, tparams, vparamss, tpt, newBlock)
          case None =>
            // create new static ctor
            val staticCtorSym = currentClass.newConstructor(template.pos)
                                  .setFlag(STATIC)
                                  .setInfo(UnitClass.tpe)
            val rhs = Block(symbolInitTrees, Literal(()))
            val staticCtorTree = DefDef(staticCtorSym, rhs)
            theTyper.typed { atPos(template.pos)(staticCtorTree) }
        }
        treeCopy.Template(template, template.parents, template.self, newCtor :: template.body)
      }
    }
  } // CleanUpTransformer

}
