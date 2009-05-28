/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author Martin Odersky
 */
// $Id$

package scala.tools.nsc.transform

import symtab._
import Flags._
import scala.tools.nsc.util.Position
import scala.collection.mutable.{ListBuffer, HashMap}

abstract class CleanUp extends Transform {
  import global._
  import definitions._

  /** the following two members override abstract members in Transform */
  val phaseName: String = "cleanup"

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new CleanUpTransformer(unit)

  class CleanUpTransformer(unit: CompilationUnit) extends Transformer {

    private val newDefs = new ListBuffer[Tree]
    private val newInits = new ListBuffer[Tree]

    private val classConstantMeth = new HashMap[String, Symbol]

    // a map from the symbols of the Scala primitive types to the symbols
    // of the modules of the Java box classes
    private val javaBoxClassModule = new HashMap[Symbol, Symbol]

    if (!forMSIL) {
      javaBoxClassModule(BooleanClass) = getModule("java.lang.Boolean")
      javaBoxClassModule(ByteClass)    = getModule("java.lang.Byte")
      javaBoxClassModule(ShortClass)   = getModule("java.lang.Short")
      javaBoxClassModule(IntClass)     = getModule("java.lang.Integer")
      javaBoxClassModule(CharClass)    = getModule("java.lang.Character")
      javaBoxClassModule(LongClass)    = getModule("java.lang.Long")
      if (!forCLDC) {
        javaBoxClassModule(FloatClass)   = getModule("java.lang.Float")
        javaBoxClassModule(DoubleClass)  = getModule("java.lang.Double")
        javaBoxClassModule(UnitClass)    = getModule("java.lang.Void")
      }
    }

    private var localTyper: analyzer.Typer = null

    private def classConstantMethod(pos: Position, sig: String): Symbol = classConstantMeth.get(sig) match {
      case Some(meth) =>
        meth
      case None =>
        val forName = getMember(ClassClass.linkedModuleOfClass, nme.forName)
        val owner = currentOwner.enclClass

        val cvar = owner.newVariable(pos, unit.fresh.newName(pos, "class$Cache"))
          .setFlag(PRIVATE | STATIC | MUTABLE | SYNTHETIC).setInfo(ClassClass.tpe)
        owner.info.decls.enter(cvar)
        val cdef =
          localTyper.typed {
            atPos(pos) {
              ValDef(cvar, Literal(Constant(null)))
            }
          }

        val meth = owner.newMethod(pos, unit.fresh.newName(pos, "class$Method"))
          .setFlag(PRIVATE | STATIC | SYNTHETIC).setInfo(MethodType(List(), ClassClass.tpe))
        owner.info.decls.enter(meth)
        val mdef =
          localTyper.typed {
            atPos(pos) {
              DefDef(meth, vparamss =>
                gen.mkCached(
                  cvar,
                  Apply(
                    gen.mkAttributedRef(forName), List(Literal(sig)))))
            }
          }

        newDefs.append(cdef, mdef);
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
       * for type-compatibility (see fixResult and fixParams).
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
        assert(ad.symbol.isPublic)
        var qual: Tree = qual0

        /* ### CREATING THE METHOD CACHE ### */

        def addStaticVariableToClass(forName: String, forType: Type, forInit: Tree): Symbol = {
          val varSym = currentClass.newVariable(ad.pos, unit.fresh.newName(ad.pos, forName))
            .setFlag(PRIVATE | STATIC | MUTABLE | SYNTHETIC)
            .setInfo(forType)
          currentClass.info.decls.enter(varSym)
          val varDef =
            localTyper.typed {
              atPos(ad.pos) {
                ValDef(varSym, forInit)
              }
            }
          newDefs.append(transform(varDef))
          val varInit =
            localTyper.typed {
              atPos(ad.pos) {
                Assign(gen.mkAttributedRef(varSym), forInit)
              }
            }
          newInits.append(transform(varInit))
          varSym
        }

        def addStaticMethodToClass(forName: String, forArgsTypes: List[Type], forResultType: Type)
                                  (forBody: Pair[Symbol, List[Symbol]] => Tree): Symbol = {
          val methSym = currentClass.newMethod(ad.pos, unit.fresh.newName(ad.pos, forName))
            .setFlag(STATIC | SYNTHETIC)
            .setInfo(MethodType(forArgsTypes, forResultType))
          currentClass.info.decls.enter(methSym)
          val methDef =
            localTyper.typed {
              atPos(ad.pos) {
                DefDef(methSym, { vparamss => forBody(Pair(methSym, vparamss(0))) })
              }
            }
          newDefs.append(transform(methDef))
          methSym
        }

        def fromTypesToClassArrayLiteral(paramTypes: List[Type]): Tree =
          ArrayValue(TypeTree(ClassClass.tpe), paramTypes map { pt => Literal(Constant(pt)) })

        def theTypeClassArray =
          TypeRef(ArrayClass.tpe.prefix, ArrayClass, List(ClassClass.tpe))

        /* ... */
        def reflectiveMethodCache(method: String, paramTypes: List[Type]): Symbol = {

          settings.refinementMethodDispatch.value match {

            case "no-cache" =>

              /* Implementation of the cache is as follows for method "def xyz(a: A, b: B)":

                var reflParams$Cache: Array[Class[_]] = Array[JClass](classOf[A], classOf[B])

                def reflMethod$Method(forReceiver: JClass[_]): JMethod =
                  forReceiver.getMethod("xyz", reflParams$Cache)

              */

              val reflParamsCacheSym: Symbol =
                addStaticVariableToClass("reflParams$Cache", theTypeClassArray, fromTypesToClassArrayLiteral(paramTypes))

              val reflMethodSym: Symbol =
                addStaticMethodToClass("reflMethod$Method", List(ClassClass.tpe), MethodClass.tpe)
                  { case Pair(reflMethodSym, List(forReceiverSym)) =>
                    Apply(
                      Select(gen.mkAttributedRef(forReceiverSym), Class_getMethod),
                      List(
                        Literal(Constant(method)),
                        gen.mkAttributedRef(reflParamsCacheSym)
                      )
                    )
                  }

              reflMethodSym

            case "mono-cache" =>

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
                addStaticVariableToClass("reflParams$Cache", theTypeClassArray, fromTypesToClassArrayLiteral(paramTypes))

              val reflMethodCacheSym: Symbol =
                addStaticVariableToClass("reflMethod$Cache", MethodClass.tpe, Literal(Constant(null)))

              val reflClassCacheSym: Symbol =
                addStaticVariableToClass("reflClass$Cache", ClassClass.tpe, Literal(Constant(null)))


              val reflMethodSym: Symbol =
                addStaticMethodToClass("reflMethod$Method", List(ClassClass.tpe), MethodClass.tpe)
                  { case Pair(reflMethodSym, List(forReceiverSym)) =>
                    Block(
                      List(
                        If(Apply(Select(gen.mkAttributedRef(reflClassCacheSym), nme.ne), List(gen.mkAttributedRef(forReceiverSym))),
                          Block(
                            List(
                              Assign(
                                gen.mkAttributedRef(reflMethodCacheSym),
                                Apply(
                                  Select(
                                    gen.mkAttributedRef(forReceiverSym),
                                    ClassClass.tpe.member(nme.getMethod_)
                                  ),
                                  List(
                                    Literal(Constant(method)),
                                    gen.mkAttributedRef(reflParamsCacheSym)
                                  )
                                )
                              ),
                              Assign(gen.mkAttributedRef(reflClassCacheSym), gen.mkAttributedRef(forReceiverSym))
                            ),
                            Literal(Constant(()))
                          ),
                          EmptyTree
                        )
                      ),
                      gen.mkAttributedRef(reflMethodCacheSym)
                    )
                  }

              reflMethodSym

            case "poly-cache" =>

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
                addStaticVariableToClass("reflParams$Cache", theTypeClassArray, fromTypesToClassArrayLiteral(paramTypes))

              val reflPolyCacheSym: Symbol =
                addStaticVariableToClass("reflPoly$Cache", MethodCacheClass.tpe, New(TypeTree(EmptyMethodCacheClass.tpe), List(Nil)))

              val reflMethodSym: Symbol =
                addStaticMethodToClass("reflMethod$Method", List(ClassClass.tpe), MethodClass.tpe)
                  { case Pair(reflMethodSym, List(forReceiverSym)) =>
                    val methodSym = reflMethodSym.newVariable(ad.pos, newTermName(unit.fresh.newName(ad.pos, "method"))) setInfo MethodClass.tpe
                    Block(
                      List(
                        ValDef(
                          methodSym,
                          Apply(
                            Select(gen.mkAttributedRef(reflPolyCacheSym), methodCache_find),
                            List(gen.mkAttributedRef(forReceiverSym))
                          )
                        )
                      ),
                      If(
                        Apply(Select(gen.mkAttributedRef(methodSym), Object_ne), List(Literal(Constant(null)))),
                        Return(gen.mkAttributedRef(methodSym)),
                        Block(
                          List(
                            Assign(gen.mkAttributedRef(methodSym),
                              Apply(
                                Select(gen.mkAttributedRef(forReceiverSym), Class_getMethod),
                                List(
                                  Literal(Constant(method)),
                                  gen.mkAttributedRef(reflParamsCacheSym)
                                )
                              )
                            ),
                            Assign(
                              gen.mkAttributedRef(reflPolyCacheSym),
                              Apply(
                                Select(gen.mkAttributedRef(reflPolyCacheSym), methodCache_add),
                                List(gen.mkAttributedRef(forReceiverSym), gen.mkAttributedRef(methodSym))
                              )
                            )
                          ),
                          Return(gen.mkAttributedRef(methodSym))
                        )
                      )
                    )
                  }

              reflMethodSym

          }
        }

        /* ### HANDLING METHODS NORMALLY COMPILED TO OPERATORS ### */

        def mayRequirePrimitiveReplacement: Boolean = {

          def isBoxed(sym: Symbol): Boolean =
            if (forCLDC) {
              (sym isNonBottomSubClass ByteClass) ||
              (sym isNonBottomSubClass ShortClass) ||
              (sym isNonBottomSubClass CharClass) ||
              (sym isNonBottomSubClass IntClass) ||
              (sym isNonBottomSubClass LongClass)
            }
            else ((sym isNonBottomSubClass BoxedNumberClass) ||
              (!forMSIL && (sym isNonBottomSubClass BoxedCharacterClass)))

          val sym = qual.tpe.typeSymbol
          (sym == definitions.ObjectClass) || isBoxed(sym)

        }

        val testForNumber: Tree =
          gen.mkOr(
            Apply(
              TypeApply(
                gen.mkAttributedSelect(qual, definitions.Object_isInstanceOf),
                List(TypeTree(BoxedNumberClass.tpe.normalize))
              ),
              List()
            ),
            Apply(
              TypeApply(
                gen.mkAttributedSelect(qual, definitions.Object_isInstanceOf),
                List(TypeTree(BoxedCharacterClass.tpe.normalize))
              ),
              List()
            )
          )

        val testForBoolean: Tree =
          Apply(
            TypeApply(
              gen.mkAttributedSelect(qual, definitions.Object_isInstanceOf),
              List(TypeTree(BoxedBooleanClass.tpe.normalize))
            ),
            List()
          )

        val testForNumberOrBoolean: Tree = gen.mkOr(testForNumber, testForBoolean)

        def getPrimitiveReplacementForStructuralCall: PartialFunction[Name, (Symbol, Tree)] = {
        /* Unary arithmetic */
          case nme.UNARY_+ =>
            (definitions.getMember(definitions.BoxesRunTimeClass, newTermName("positive")), testForNumber)
          case nme.UNARY_- =>
            (definitions.getMember(definitions.BoxesRunTimeClass, newTermName("negate")), testForNumber)
        /* Unary logic */
          case nme.UNARY_~ =>
            (definitions.getMember(definitions.BoxesRunTimeClass, newTermName("complement")), testForNumber)
          case nme.UNARY_! =>
            (definitions.getMember(definitions.BoxesRunTimeClass, newTermName("takeNot")), testForBoolean)
        /* Binary arithmetic */
          case nme.ADD =>
            (definitions.getMember(definitions.BoxesRunTimeClass, newTermName("add")), testForNumber)
          case nme.SUB =>
            (definitions.getMember(definitions.BoxesRunTimeClass, newTermName("subtract")), testForNumber)
          case nme.MUL =>
            (definitions.getMember(definitions.BoxesRunTimeClass, newTermName("multiply")), testForNumber)
          case nme.DIV =>
            (definitions.getMember(definitions.BoxesRunTimeClass, newTermName("divide")), testForNumber)
          case nme.MOD =>
            (definitions.getMember(definitions.BoxesRunTimeClass, newTermName("takeModulo")), testForNumber)
        /* Binary logic */
          case nme.OR =>
            (definitions.getMember(definitions.BoxesRunTimeClass, newTermName("takeOr")), testForNumberOrBoolean)
          case nme.XOR =>
            (definitions.getMember(definitions.BoxesRunTimeClass, newTermName("takeXor")), testForNumberOrBoolean)
          case nme.AND =>
            (definitions.getMember(definitions.BoxesRunTimeClass, newTermName("takeAnd")), testForNumberOrBoolean)
          case nme.ZOR =>
            (definitions.getMember(definitions.BoxesRunTimeClass, newTermName("takeConditionalOr")), testForBoolean)
          case nme.ZAND =>
            (definitions.getMember(definitions.BoxesRunTimeClass, newTermName("takeConditionalAnd")), testForBoolean)
        /* Shifting */
          case nme.LSL =>
            (definitions.getMember(definitions.BoxesRunTimeClass, newTermName("shiftSignedLeft")), testForNumber)
          case nme.LSR =>
            (definitions.getMember(definitions.BoxesRunTimeClass, newTermName("shiftSignedRight")), testForNumber)
          case nme.ASR =>
            (definitions.getMember(definitions.BoxesRunTimeClass, newTermName("shiftLogicalRight")), testForNumber)
          case nme.EQ =>
            (definitions.getMember(definitions.BoxesRunTimeClass, newTermName("testEqual")), testForNumberOrBoolean)
          case nme.NE =>
            (definitions.getMember(definitions.BoxesRunTimeClass, newTermName("testNotEqual")), testForNumberOrBoolean)
          case nme.LT =>
            (definitions.getMember(definitions.BoxesRunTimeClass, newTermName("testLessThan")), testForNumber)
          case nme.LE =>
            (definitions.getMember(definitions.BoxesRunTimeClass, newTermName("testLessOrEqualThan")), testForNumber)
          case nme.GE =>
            (definitions.getMember(definitions.BoxesRunTimeClass, newTermName("testGreaterOrEqualThan")), testForNumber)
          case nme.GT =>
            (definitions.getMember(definitions.BoxesRunTimeClass, newTermName("testGreaterThan")), testForNumber)
        /* Conversions */
          case nme.toByte =>
            (definitions.getMember(definitions.BoxesRunTimeClass, newTermName("toByte")), testForNumber)
          case nme.toShort =>
            (definitions.getMember(definitions.BoxesRunTimeClass, newTermName("toShort")), testForNumber)
          case nme.toChar =>
            (definitions.getMember(definitions.BoxesRunTimeClass, newTermName("toCharacter")), testForNumber)
          case nme.toInt =>
            (definitions.getMember(definitions.BoxesRunTimeClass, newTermName("toInteger")), testForNumber)
          case nme.toLong =>
            (definitions.getMember(definitions.BoxesRunTimeClass, newTermName("toLong")), testForNumber)
          case nme.toFloat =>
            (definitions.getMember(definitions.BoxesRunTimeClass, newTermName("toFloat")), testForNumber)
          case nme.toDouble =>
            (definitions.getMember(definitions.BoxesRunTimeClass, newTermName("toDouble")), testForNumber)
        }

        /* ### BOXING PARAMS & UNBOXING RESULTS ### */

        /* Transforms the result of a reflective call (always an AnyRef) to
         * the actual result value (an AnyRef too). The transformation
         * depends on the method's static return type.
         * - for units (void), the reflective call will return null: a new
         *   boxed unit is generated.
         * - for arrays, the reflective call will return an unboxed array:
         *   the resulting array is boxed.
         * - otherwise, the value is simply casted to the expected type. This
         *   is enough even for value (int et al.) values as the result of
         *   a dynamic call will box them as a side-effect. */
        def fixResult(resType: Type)(tree: Tree): Tree =
          localTyper.typed {
            if (resType.typeSymbol == UnitClass)
              Block (
                List(tree),
                gen.mkAttributedRef(BoxedUnit_UNIT)
              )
            else if (resType.typeSymbol == ArrayClass) {
              val sym = currentOwner.newValue(ad.pos, newTermName(unit.fresh.newName(ad.pos))) setInfo ObjectClass.tpe
              Block(
                List(ValDef(sym, tree)),
                If(
                  Apply(Select(Literal(Constant(null)), Any_==), List(gen.mkAttributedRef(sym))),
                  Literal(Constant(null)),
                  Apply(
                    Select(
                      gen.mkAttributedRef(ScalaRunTimeModule),
                      ScalaRunTimeModule.tpe.member(nme.boxArray)
                    ),
                    List(gen.mkAttributedRef(sym))
                  )
                )
              )
            }
            else if (resType.typeSymbol == ObjectClass) // TODO: remove the cast always when unnecessary.
              tree
            else
              gen.mkAttributedCast(tree, resType)
          }

        /* Transforms the parameters of a dynamic apply (always AnyRefs) to
         * something compatible with reclective calls. The transformation depends
         * on the method's static parameter types.
         * - for (unboxed) arrays, the (non-null) value is tested for its erased
         *   type. If it is a boxed array, the array is unboxed. If it is an
         *   unboxed array, it is left alone. */
        def fixParams(params: List[Tree], paramTypes: List[Type]): List[Tree] =
          (params zip paramTypes) map { case (param, paramType) =>
            localTyper.typed {
              if (paramType.typeSymbol == ArrayClass) {
                val sym = currentOwner.newValue(ad.pos, newTermName(unit.fresh.newName(ad.pos))) setInfo ObjectClass.tpe
                val arrayType = {
                  assert(paramType.typeArgs.length == 1)
                  paramType.typeArgs(0).normalize
                }
                Block(
                  List(ValDef(sym, param)),
                  If(
                    Apply(Select(Literal(Constant(null)), Any_==), List(gen.mkAttributedRef(sym))),
                    Literal(Constant(null)),
                    If(
                      Apply(
                        TypeApply(
                          gen.mkAttributedSelect(gen.mkAttributedRef(sym), definitions.Object_isInstanceOf),
                          List(TypeTree(BoxedArrayClass.tpe.normalize))
                        ),
                        List()
                      ),
                      Apply(
                        Select(gen.mkAttributedCast(gen.mkAttributedRef(sym), BoxedArrayClass.tpe), getMember(BoxedArrayClass, nme.unbox)),
                        List(Literal(Constant(arrayType)))
                      ),
                      gen.mkAttributedRef(sym)
                    )
                  )
                )
              }
              else
                param
            }
          }

        /* ### CALLING THE APPLY -> one for operators (see above), one for normal methods ### */

        def callAsOperator(paramTypes: List[Type], resType: Type): Tree = localTyper.typed {
          if (getPrimitiveReplacementForStructuralCall isDefinedAt ad.symbol.name) {
            val (operator, test) = getPrimitiveReplacementForStructuralCall(ad.symbol.name)
            If(
              test,
              Apply(
                gen.mkAttributedRef(operator),
                qual :: fixParams(params, paramTypes)
              ),
              callAsMethod(paramTypes, resType)
            )
          }
          else callAsMethod(paramTypes, resType)
        }

        def callAsMethod(paramTypes: List[Type], resType: Type): Tree = localTyper.typed {
          val invokeExc =
            currentOwner.newValue(ad.pos, newTermName(unit.fresh.newName(ad.pos))) setInfo InvocationTargetExceptionClass.tpe
          Try(
            Apply(
              Select(
                Apply(
                  gen.mkAttributedRef(reflectiveMethodCache(ad.symbol.name.toString, paramTypes)),
                  List(Apply(Select(qual, ObjectClass.tpe.member(nme.getClass_)), Nil))
                ),
                MethodClass.tpe.member(nme.invoke_)
              ),
              List(
                qual,
                ArrayValue(TypeTree(ObjectClass.tpe), fixParams(params, paramTypes))
              )
            ),
            List(CaseDef(
              Bind(invokeExc, Typed(Ident(nme.WILDCARD), TypeTree(InvocationTargetExceptionClass.tpe))),
              EmptyTree,
              Throw(Apply(Select(Ident(invokeExc), nme.getCause), Nil))
            )),
            EmptyTree
          )
        }

        def getClass(q: Tree): Tree =
          Apply(Select(q, nme.getClass_), List())

      if (settings.refinementMethodDispatch.value == "invoke-dynamic") {
/*        val guardCallSite: Tree = {
          val cachedClass = addStaticVariableToClass("cachedClass", definitions.ClassClass.tpe, EmptyTree)
          val tmpVar = currentOwner.newVariable(ad.pos, unit.fresh.newName(ad.pos, "x")).setInfo(definitions.AnyRefClass.tpe)
          atPos(ad.pos)(Block(List(
            ValDef(tmpVar, transform(qual))),
            If(Apply(Select(gen.mkAttributedRef(cachedClass), nme.EQ), List(getClass(Ident(tmpVar)))),
               Block(List(Assign(gen.mkAttributedRef(cachedClass), getClass(Ident(tmpVar)))),
                     copy.ApplyDynamic(ad, Ident(tmpVar), transformTrees(params))),
               EmptyTree)))
        }
        //println(guardCallSite)
*/
        localTyper.typed(copy.ApplyDynamic(ad, transform(qual), transformTrees(params)))
      } else {

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
         *   On the other hand, arrays must be dealt with as they must be entered
         *   unboxed in the parameter array of invoke. fixParams is responsible for
         *   that.
         * - in the end, the result of invoke must be fixed, again to deal with arrays.
         *   This is provided by fixResult. fixResult will cast the invocation's result
         *   to the method's return type, which is generally ok, except when this type
         *   is a value type (int et al.) in which case it must cast to the boxed version
         *   because invoke only returns object and erasure made sure the result is
         *   expected to be an AnyRef. */
        val t: Tree = ad.symbol.tpe match {
          case MethodType(paramTypes, resType) =>
            assert(params.length == paramTypes.length)
            atPos(ad.pos)(localTyper.typed {
              val t1 = newTermName(unit.fresh.newName(ad.pos, "qual"))
              val sym = currentOwner.newValue(ad.pos, t1) setInfo qual0.tpe
              qual = gen.mkAttributedRef(sym)
              Block(
                List(ValDef(sym, qual0)),
                fixResult(if (isValueClass(resType.typeSymbol)) boxedClass(resType.typeSymbol).tpe else resType) {
                  if (mayRequirePrimitiveReplacement)
                    callAsOperator(paramTypes, resType)
                  else
                    callAsMethod(paramTypes, resType)
                }
              )
            })
        }

        /* For testing purposes, the dynamic application's condition
         * can be printed-out in great detail. Remove? */
        if (settings.debug.value) {
          Console.println(
            "Dynamically applying '" + qual + "." + ad.symbol.name +
            "(" + params.map(_.toString).mkString(", ") + ")' with"
          )
          ad.symbol.tpe match {
            case MethodType(paramTypes, resType) =>
              Console.println(
                "  - declared parameters' types: " +
                (paramTypes.map(_.toString)).mkString("'",", ","'"))
              Console.println(
                "  - passed arguments' types:    " +
                (params.map(_.toString)).mkString("'",", ","'"))
              Console.println(
                "  - result type:                '" +
                resType.toString + "'")
          }
          Console.println("  - resulting code:    '" + t + "'")
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
                      copy.Block(theRhs, transformTrees(newInits.toList) ::: stats, expr)
                  }
                  copy.DefDef(thePrimaryConstructor, mods, name, tparams, vparamss, tpt, newRhs)
                case notThePrimaryConstructor =>
                  notThePrimaryConstructor
              }
            )
            copy.Template(tree, parents, self, newBody)
        }
        else super.transform(tree)

      case Literal(c) if (c.tag == ClassTag) && !forMSIL=>
        val tpe = c.typeValue
        atPos(tree.pos) {
          localTyper.typed {
            if ((isValueClass(tpe.typeSymbol) || tpe.typeSymbol == definitions.UnitClass)
                && !forCLDC)
              Select(gen.mkAttributedRef(javaBoxClassModule(tpe.typeSymbol)), "TYPE")
            else tree
          }
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

        val newBlock = super.transform(Block(Nil, Assign(Ident(tempVar), transform(block))))
        val newCatches = for (CaseDef(pattern, guard, body) <- catches) yield {
          CaseDef(
            super.transform(pattern),
            super.transform(guard),
            Block(Nil, Assign(Ident(tempVar), super.transform(body)))
          )
        }
        val newTry = Try(newBlock, newCatches, super.transform(finalizer))
        val res = Block(List(ValDef(tempVar, EmptyTree), newTry), Ident(tempVar))
        localTyper.typed(res)

      /* Adds @serializable annotation to anonymous function classes */
      case cdef @ ClassDef(mods, name, tparams, impl) =>
        if (settings.target.value == "jvm-1.5") {
          val sym = cdef.symbol
          // is this an anonymous function class?
          if (sym.isAnonymousFunction && !sym.hasAttribute(SerializableAttr))
            sym.attributes =
              AnnotationInfo(definitions.SerializableAttr.tpe, List(), List()) :: sym.attributes
        }
        super.transform(tree)

      case _ =>
        super.transform(tree)
    }
  } // CleanUpTransformer

}
