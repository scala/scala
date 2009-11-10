/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author Martin Odersky
 */
// $Id$

package scala.tools.nsc
package transform

import symtab._
import Flags._
import scala.tools.nsc.util.Position
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

    private def classConstantMethod(pos: Position, sig: String): Symbol =
      (classConstantMeth get sig) getOrElse {
        val forName = getMember(ClassClass.linkedModuleOfClass, nme.forName)
        val owner = currentOwner.enclClass

        val cvar = owner.newVariable(pos, unit.fresh.newName(pos, "class$Cache"))
          .setFlag(PRIVATE | STATIC | MUTABLE | SYNTHETIC).setInfo(ClassClass.tpe)
        owner.info.decls enter cvar
        val cdef = typedWithPos(pos) { VAL(cvar) === NULL }

        val meth = owner.newMethod(pos, unit.fresh.newName(pos, "class$Method"))
          .setFlag(PRIVATE | STATIC | SYNTHETIC).setInfo(MethodType(List(), ClassClass.tpe))
        owner.info.decls enter meth
        val mdef = typedWithPos(pos)(DEF(meth) ===
          gen.mkCached(cvar, Apply(REF(forName), List(Literal(sig))))
        )

        newDefs.append(cdef, mdef)
        classConstantMeth.update(sig, meth)
        meth
      }

    override def transformUnit(unit: CompilationUnit) =
      unit.body = transform(unit.body)

    /** A value class is defined to be only Java-compatible values: unit is
      * not part of it, as opposed to isValueClass in definitions. scala.Int is
      * a value class, java.lang.Integer is not. */
    def isValueClass(sym: Symbol) = boxedClass contains sym

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
       *   method (dispatching on paramters is static in Scala). In practice,
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

              /* Implementation of the cache is as follows for method "def xyz(a: A, b: B)":

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
                addStaticVariableToClass("reflClass$Cache", ClassClass.tpe, NULL, false)

              def getMethodSym = ClassClass.tpe member nme.getMethod_

              addStaticMethodToClass("reflMethod$Method", List(ClassClass.tpe), MethodClass.tpe) {
                case Pair(reflMethodSym, List(forReceiverSym)) =>
                  BLOCK(
                    IF (REF(reflClassCacheSym) ANY_NE REF(forReceiverSym)) THEN BLOCK(
                      REF(reflMethodCacheSym) === ((REF(forReceiverSym) DOT getMethodSym)(LIT(method), REF(reflParamsCacheSym))) ,
                      REF(reflClassCacheSym) === REF(forReceiverSym),
                      UNIT
                    ) ENDIF,
                    REF(reflMethodCacheSym)
                  )
              }

            case POLY_CACHE =>

              /* Implementation of the cache is as follows for method "def xyz(a: A, b: B)":

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

              val reflPolyCacheSym: Symbol =
                addStaticVariableToClass("reflPoly$Cache", MethodCacheClass.tpe, NEW(TypeTree(EmptyMethodCacheClass.tpe)), false)

              addStaticMethodToClass("reflMethod$Method", List(ClassClass.tpe), MethodClass.tpe)
                { case Pair(reflMethodSym, List(forReceiverSym)) =>
                  val methodSym = reflMethodSym.newVariable(ad.pos, mkTerm("method")) setInfo MethodClass.tpe

                  BLOCK(
                    VAL(methodSym) === ((REF(reflPolyCacheSym) DOT methodCache_find)(REF(forReceiverSym))) ,
                    IF (REF(methodSym) OBJ_!= NULL) .
                      THEN (Return(REF(methodSym)))
                    ELSE {
                      def methodSymRHS  = ((REF(forReceiverSym) DOT Class_getMethod)(LIT(method), REF(reflParamsCacheSym)))
                      def cacheRHS      = ((REF(reflPolyCacheSym) DOT methodCache_add)(REF(forReceiverSym), REF(methodSym)))
                      BLOCK(
                        REF(methodSym)        === methodSymRHS,
                        REF(reflPolyCacheSym) === cacheRHS,
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
        def callAsReflective(paramTypes: List[Type], resType: Type, structResType: Type): Tree = localTyper typed {
          def fixResult(tree: Tree): Tree = localTyper typed {
            structResType.typeSymbol match {
              case UnitClass    => BLOCK(tree, REF(BoxedUnit_UNIT))
              case ObjectClass  => tree
              case _            => tree AS_ATTR structResType
            }
          }
          val qualSym = qual.tpe.typeSymbol
          val methSym = ad.symbol
          def defaultCall = {
            // reflective method call machinery
            val invokeName  = MethodClass.tpe member nme.invoke_    // reflect.Method.invoke(...)
            def cache       = REF(reflectiveMethodCache(ad.symbol.name.toString, paramTypes)) // cache Symbol
            def lookup      = Apply(cache, List(qual GETCLASS))     // get Method object from cache
            def args        = ArrayValue(TypeTree(ObjectClass.tpe), params)  // args for invocation
            def invocation  = (lookup DOT invokeName)(qual, args)   // .invoke(qual, ...)

            // exception catching machinery
            val invokeExc   = currentOwner.newValue(ad.pos, mkTerm()) setInfo InvocationTargetExceptionClass.tpe
            def catchVar    = Bind(invokeExc, Typed(Ident(nme.WILDCARD), TypeTree(InvocationTargetExceptionClass.tpe)))
            def catchBody   = Throw(Apply(Select(Ident(invokeExc), nme.getCause), Nil))

            // try { method.invoke } catch { case e: InvocationTargetExceptionClass => throw e.getCause() }
            TRY (invocation) CATCH { CASE (catchVar) ==> catchBody } ENDTRY
          }
          def useValueOperator = {
            def isBoxed(qualSym: Symbol): Boolean =
              (qualSym isNonBottomSubClass BoxedNumberClass) ||
              (!forMSIL && (qualSym isNonBottomSubClass BoxedCharacterClass))
            ((qualSym == definitions.ObjectClass) || isBoxed(qualSym)) && // may be a boxed value class
            (getPrimitiveReplacementForStructuralCall isDefinedAt methSym.name) &&
            ((resType :: paramTypes) forall (x => isValueClass(x.typeSymbol))) // issue #1110
          }
          def useArrayOperator =
            ((qualSym == definitions.ObjectClass) || (qualSym == definitions.ArrayClass)) &&
            ((methSym.name == nme.length) || (methSym.name == nme.update) || (methSym.name == nme.apply))
          val callCode = if (useValueOperator) {
            val (operator, test)  = getPrimitiveReplacementForStructuralCall(methSym.name)
            def args              = qual :: params
            fixResult((IF (test) THEN (REF(operator) APPLY args) ELSE defaultCall))
          }
          else if (useArrayOperator) {
            val args = qual :: params
            val operatorCall = // what follows is incredibly ugly. this dirty fix should be deal with at the next cleanup of cleanup.
              if (methSym.name == nme.length)
                (REF(boxMethod(IntClass)) APPLY (REF(arrayLengthMethod) APPLY args))
              else if (methSym.name == nme.update)
                (REF(arrayUpdateMethod) APPLY List(args(0), (REF(unboxMethod(IntClass)) APPLY args(1)), args(2)))
              else
                (REF(arrayApplyMethod) APPLY List(args(0), (REF(unboxMethod(IntClass)) APPLY args(1))))
            (IF (qual IS_OBJ arrayType(ObjectClass.tpe)) THEN operatorCall
            ELSE (IF (qual IS_OBJ arrayType(ByteClass.tpe)) THEN operatorCall
            ELSE (IF (qual IS_OBJ arrayType(ShortClass.tpe)) THEN operatorCall
            ELSE (IF (qual IS_OBJ arrayType(IntClass.tpe)) THEN operatorCall
            ELSE (IF (qual IS_OBJ arrayType(LongClass.tpe)) THEN operatorCall
            ELSE (IF (qual IS_OBJ arrayType(FloatClass.tpe)) THEN operatorCall
            ELSE (IF (qual IS_OBJ arrayType(DoubleClass.tpe)) THEN operatorCall
            ELSE (IF (qual IS_OBJ arrayType(CharClass.tpe)) THEN operatorCall
            ELSE (IF (qual IS_OBJ arrayType(BooleanClass.tpe)) THEN operatorCall
            ELSE fixResult(defaultCall)
            )))))))))
          }
          else fixResult(defaultCall)
          localTyper.typed(callCode)
        }

        def getClass(q: Tree): Tree = (q DOT nme.getClass_)()

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
           * of three succesive calls, first to getClass on the callee, then to
           * getMethod on the classs, then to invoke on the method.
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

                def structResType = if (isValueClass(resType.typeSymbol)) boxedClass(resType.typeSymbol).tpe else resType
                BLOCK(
                  VAL(sym) === qual0,
                  callAsReflective(mparams map (_.tpe), resType, structResType)
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
        if (!forMSIL) {
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

      case Literal(c) if (c.tag == ClassTag) && !forMSIL=>
        val tpe = c.typeValue
        typedWithPos(tree.pos) {
          if (isValueClass(tpe.typeSymbol) || tpe.typeSymbol == definitions.UnitClass) {
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

      case _ =>
        super.transform(tree)
    }
  } // CleanUpTransformer

}
