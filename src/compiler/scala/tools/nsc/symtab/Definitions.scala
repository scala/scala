/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc
package symtab

import scala.collection.mutable.{HashMap, HashSet}
import scala.tools.nsc.util.{Position, NoPosition}
import Flags._

trait Definitions {
  self: SymbolTable =>

  object definitions {
    def isDefinitionsInitialized = isInitialized

    // Working around bug #2133
    private object definitionHelpers {
      def cond[T](x: T)(f: PartialFunction[T, Boolean]) = (f isDefinedAt x) && f(x)
    }
    import definitionHelpers._

    // symbols related to packages
    var emptypackagescope: Scope = null //debug

    lazy val RootPackage: Symbol = {
      val rp=NoSymbol.newValue(NoPosition, nme.ROOTPKG)
        .setFlag(FINAL | MODULE | PACKAGE | JAVA)
        .setInfo(PolyType(List(), RootClass.tpe))
      RootClass.setSourceModule(rp)
      rp
    }
    lazy val RootClass: ModuleClassSymbol = NoSymbol.newModuleClass(NoPosition, nme.ROOT.toTypeName)
          .setFlag(FINAL | MODULE | PACKAGE | JAVA).setInfo(rootLoader)

    lazy val EmptyPackage       = RootClass.newPackage(NoPosition, nme.EMPTY_PACKAGE_NAME).setFlag(FINAL)
    lazy val EmptyPackageClass  = EmptyPackage.moduleClass

    lazy val JavaLangPackage    = getModule(sn.JavaLang)
    lazy val ScalaPackage       = getModule("scala")
    lazy val ScalaPackageClass  = ScalaPackage.tpe.typeSymbol

    lazy val ScalaCollectionImmutablePackage: Symbol = getModule("scala.collection.immutable")
    lazy val ScalaCollectionImmutablePackageClass: Symbol = ScalaCollectionImmutablePackage.tpe.typeSymbol

    // convenient one-argument parameter lists
    lazy val anyparam     = List(AnyClass.typeConstructor)
    lazy val anyvalparam  = List(AnyValClass.typeConstructor)
    lazy val anyrefparam  = List(AnyRefClass.typeConstructor)

    // top types
    lazy val AnyClass     = newClass(ScalaPackageClass, nme.Any, Nil) setFlag (ABSTRACT)
    lazy val AnyValClass  = newClass(ScalaPackageClass, nme.AnyVal, anyparam) setFlag (FINAL | SEALED)
    lazy val AnyRefClass  = newAlias(ScalaPackageClass, nme.AnyRef, ObjectClass.typeConstructor)
    lazy val ObjectClass  = getClass(sn.Object)

    // bottom types
    lazy val NullClass    = newClass(ScalaPackageClass, nme.Null, anyrefparam) setFlag (ABSTRACT | TRAIT | FINAL)
    lazy val NothingClass = newClass(ScalaPackageClass, nme.Nothing, anyparam) setFlag (ABSTRACT | TRAIT | FINAL)
    lazy val RuntimeNothingClass  = getClass("scala.runtime.Nothing$")
    lazy val RuntimeNullClass     = getClass("scala.runtime.Null$")

    // the scala value classes
    lazy val UnitClass    = newClass(ScalaPackageClass, nme.Unit, anyvalparam).setFlag(ABSTRACT | FINAL)
    lazy val ByteClass    = newValueClass(nme.Byte, 'B')
    lazy val ShortClass   = newValueClass(nme.Short, 'S')
    lazy val CharClass    = newValueClass(nme.Char, 'C')
    lazy val IntClass     = newValueClass(nme.Int, 'I')
    lazy val LongClass    = newValueClass(nme.Long, 'L')
    lazy val FloatClass   = newValueClass(nme.Float, 'F')
    lazy val DoubleClass  = newValueClass(nme.Double, 'D')
    lazy val BooleanClass = newValueClass(nme.Boolean, 'Z')
      def Boolean_and = getMember(BooleanClass, nme.ZAND)
      def Boolean_or  = getMember(BooleanClass, nme.ZOR)

    // exceptions and other throwables
    lazy val ThrowableClass                 = getClass(sn.Throwable)
    lazy val NullPointerExceptionClass      = getClass(sn.NPException)
    lazy val NonLocalReturnExceptionClass   = getClass(sn.NLRException)
    lazy val IndexOutOfBoundsExceptionClass = getClass(sn.IOOBException)
    lazy val UninitializedErrorClass        = getClass("scala.UninitializedFieldError")
    lazy val MatchErrorClass                = getClass("scala.MatchError")
    // java is hard coded because only used by structural values
    lazy val InvocationTargetExceptionClass = getClass("java.lang.reflect.InvocationTargetException")

    // annotations
    lazy val AnnotationClass            = getClass("scala.Annotation")
    lazy val ClassfileAnnotationClass   = getClass("scala.ClassfileAnnotation")
    lazy val StaticAnnotationClass      = getClass("scala.StaticAnnotation")
    lazy val uncheckedStableClass       = getClass("scala.annotation.unchecked.uncheckedStable")
    lazy val uncheckedVarianceClass     = getClass("scala.annotation.unchecked.uncheckedVariance")
    lazy val UncheckedClass             = getClass("scala.unchecked")
    lazy val TailrecClass               = getClass("scala.annotation.tailrec")
    lazy val SwitchClass                = getClass("scala.annotation.switch")
    lazy val ExperimentalClass          = getClass("scala.annotation.experimental")

    // fundamental reference classes
    lazy val ScalaObjectClass     = getClass("scala.ScalaObject")
    lazy val PartialFunctionClass = getClass("scala.PartialFunction")
    lazy val StringClass          = getClass(sn.String)
    lazy val ClassClass           = getClass(sn.Class)
      def Class_getMethod = getMember(ClassClass, nme.getMethod_)

    // fundamental modules
    lazy val PredefModule: Symbol = getModule("scala.Predef")
      def Predef_classOf = getMember(PredefModule, nme.classOf)
      def Predef_classOfType(classType: Type): Type =
        if (!ClassClass.unsafeTypeParams.isEmpty && !phase.erasedTypes)
          appliedType(ClassClass.tpe, List(classType))
        else ClassClass.tpe
      def Predef_identity = getMember(PredefModule, nme.identity)
      def Predef_error    = getMember(PredefModule, nme.error)
    lazy val ConsoleModule: Symbol = getModule("scala.Console")
    lazy val ScalaRunTimeModule: Symbol = getModule("scala.runtime.ScalaRunTime")
      def SeqFactory = getMember(ScalaRunTimeModule, nme.Seq)
      def checkDefinedMethod = getMember(ScalaRunTimeModule, "checkDefined")
      def isArrayMethod = getMember(ScalaRunTimeModule, "isArray")

    // classes with special meanings
    lazy val NotNullClass         = getClass("scala.NotNull")
    lazy val TypeConstraintClass  = getClass("scala.TypeConstraint")
    lazy val SingletonClass       = newClass(ScalaPackageClass, nme.Singleton, anyparam) setFlag (ABSTRACT | TRAIT | FINAL)
    lazy val SerializableClass    = getClass(sn.Serializable)

    lazy val RepeatedParamClass = newCovariantPolyClass(
      ScalaPackageClass,
      nme.REPEATED_PARAM_CLASS_NAME,
      tparam => seqType(tparam.typeConstructor)
    )
    lazy val ByNameParamClass = newCovariantPolyClass(
      ScalaPackageClass,
      nme.BYNAME_PARAM_CLASS_NAME,
      tparam => AnyClass.typeConstructor
    )
    lazy val EqualsPatternClass = {
      val clazz = newClass(ScalaPackageClass, nme.EQUALS_PATTERN_NAME, Nil)
      clazz setInfo PolyType(List(newTypeParam(clazz, 0)), ClassInfoType(anyparam, newClassScope(clazz), clazz))

      clazz
    }

    // collections classes
    lazy val IteratorClass        = getClass2("scala.Iterator", "scala.collection.Iterator")
    lazy val TraversableClass     = getClass("scala.collection.Traversable")
    lazy val IterableClass        = getClass2("scala.Iterable", "scala.collection.Iterable")
      def Iterable_next     = getMember(IterableClass, nme.next)
      def Iterable_hasNext  = getMember(IterableClass, nme.hasNext)

    lazy val SeqClass   = getClass2("scala.Seq", "scala.collection.Sequence")
    lazy val SeqModule  = getModule2("scala.Seq", "scala.collection.Sequence")
      def Seq_length = getMember(SeqClass, nme.length)
    lazy val RandomAccessSeqMutableClass = getMember(
      getModule2("scala.RandomAccessSeq", "scala.collection.Vector"), nme.Mutable)

    lazy val ListModule   = getModule2("scala.List", "scala.collection.immutable.List")
      def List_apply = getMember(ListModule, nme.apply)
    lazy val ListClass    = getClass2("scala.List", "scala.collection.immutable.List")
      def List_isEmpty  = getMember(ListClass, nme.isEmpty)
      def List_head     = getMember(ListClass, nme.head)
      def List_tail     = getMember(ListClass, nme.tail)
    lazy val ConsClass    = getClass2("scala.$colon$colon", "scala.collection.immutable.$colon$colon")
    lazy val NilModule    = getModule2("scala.Nil", "scala.collection.immutable.Nil")

    lazy val ArrayClass   = getClass("scala.Array")
      def Array_apply   = getMember(ArrayClass, nme.apply)
      def Array_update  = getMember(ArrayClass, nme.update)
    lazy val ArrayModule  = getModule("scala.Array")
      def ArrayModule_apply = getMember(ArrayModule, nme.apply)

    // reflection / structural types
    lazy val MethodClass            = getClass(sn.MethodAsObject)
    lazy val EmptyMethodCacheClass  = getClass("scala.runtime.EmptyMethodCache")
    lazy val MethodCacheClass       = getClass("scala.runtime.MethodCache")
      def methodCache_find  = getMember(MethodCacheClass, nme.find_)
      def methodCache_add   = getMember(MethodCacheClass, nme.add_)

    // scala.reflect
    lazy val ManifestClass        = getClass("scala.reflect.Manifest")
    lazy val ManifestModule       = getModule("scala.reflect.Manifest")
    lazy val OptManifestClass     = getClass("scala.reflect.OptManifest")
    lazy val NoManifest           = getModule("scala.reflect.NoManifest")
    lazy val CodeClass            = getClass(sn.Code)
    lazy val CodeModule           = getModule(sn.Code)
      def Code_lift = getMember(CodeModule, nme.lift_)

    // invoke dynamic support
    lazy val LinkageModule = getModule("java.dyn.Linkage")
      lazy val Linkage_invalidateCallerClass = getMember(LinkageModule, "invalidateCallerClass")
    lazy val DynamicDispatchClass = getModule("scala.runtime.DynamicDispatch")
      lazy val DynamicDispatch_DontSetTarget = getMember(DynamicDispatchClass, "DontSetTarget")

    // Option classes
    lazy val OptionClass: Symbol  = getClass("scala.Option")
    lazy val SomeClass: Symbol    = getClass("scala.Some")
    lazy val NoneClass: Symbol    = getModule("scala.None")

    def isOptionType(tp: Type)  = cond(tp.normalize) { case TypeRef(_, OptionClass, List(_)) => true }
    def isSomeType(tp: Type)    = cond(tp.normalize) { case TypeRef(_,   SomeClass, List(_)) => true }
    def isNoneType(tp: Type)    = cond(tp.normalize) { case TypeRef(_,   NoneClass, List(_)) => true }

    def optionType(tp: Type)    = typeRef(OptionClass.typeConstructor.prefix, OptionClass, List(tp))
    def someType(tp: Type)      = typeRef(SomeClass.typeConstructor.prefix, SomeClass, List(tp))

    // Product, Tuple, Function
    private def mkArityArray(name: String, arity: Int, countFrom: Int = 1) = {
      val list = (countFrom to arity).toList map (i => getClass(("scala." + name + i): Name))
      if (countFrom == 0) list.toArray
      else (NoSymbol :: list).toArray
    }

    val MaxTupleArity, MaxProductArity, MaxFunctionArity = 22
    lazy val TupleClass     = mkArityArray("Tuple", MaxTupleArity)
    lazy val ProductClass   = mkArityArray("Product", MaxProductArity)
    lazy val FunctionClass  = mkArityArray("Function", MaxFunctionArity, 0)

      def tupleField(n: Int, j: Int) = getMember(TupleClass(n), "_" + j)
      def isTupleType(tp: Type): Boolean = cond(tp.normalize) {
        case TypeRef(_, sym, elems) => elems.length <= MaxTupleArity && sym == TupleClass(elems.length)
      }
      def tupleType(elems: List[Type]) =
        if (elems.length <= MaxTupleArity) {
          val sym = TupleClass(elems.length)
          typeRef(sym.typeConstructor.prefix, sym, elems)
        } else NoType;

    lazy val ProductRootClass: Symbol = getClass("scala.Product")
      def Product_productArity = getMember(ProductRootClass, nme.productArity)
      def Product_productElement = getMember(ProductRootClass, nme.productElement)
      def Product_productPrefix = getMember(ProductRootClass, nme.productPrefix)
      def Product_canEqual = getMember(ProductRootClass, nme.canEqual_)

      def productProj(z:Symbol, j: Int): Symbol = getMember(z, nme.Product_(j))
      def productProj(n: Int,   j: Int): Symbol = productProj(ProductClass(n), j)

      /** returns true if this type is exactly ProductN[T1,...,Tn], not some subclass */
      def isExactProductType(tp: Type): Boolean = cond(tp.normalize) {
        case TypeRef(_, sym, elems) => elems.length <= MaxProductArity && sym == ProductClass(elems.length)
      }

      def productType(elems: List[Type]) =
        if (elems.isEmpty)
          UnitClass.tpe
        else if (elems.length <= MaxProductArity) {
          val sym = ProductClass(elems.length)
          typeRef(sym.typeConstructor.prefix, sym, elems)
        } else NoType

    /** if tpe <: ProductN[T1,...,TN], returns Some(T1,...,TN) else None */
    def getProductArgs(tpe: Type): Option[List[Type]] =
      tpe.baseClasses.find(x => isExactProductType(x.tpe)) match {
        case Some(p) => Some(tpe.baseType(p).typeArgs)
        case _       => None
      }

    def unapplyUnwrap(tpe:Type) = (tpe match {
      case PolyType(_,MethodType(_, res)) => res
      case MethodType(_, res)             => res
      case tpe                            => tpe
    }).normalize

    def functionApply(n: Int) = getMember(FunctionClass(n), nme.apply)
    def functionType(formals: List[Type], restpe: Type) =
      if (formals.length <= MaxFunctionArity) {
        val sym = FunctionClass(formals.length)
        typeRef(sym.typeConstructor.prefix, sym, formals ::: List(restpe))
      } else NoType

    def isFunctionType(tp: Type): Boolean = tp.normalize match {
      case TypeRef(_, sym, args) =>
        (args.length > 0) && (args.length - 1 <= MaxFunctionArity) &&
        (sym == FunctionClass(args.length - 1))
      case _ =>
        false
    }

    def seqType(arg: Type) = typeRef(SeqClass.typeConstructor.prefix, SeqClass, List(arg))

    //
    // .NET backend
    //
    // System.ValueType
    lazy val ValueTypeClass: Symbol = getClass(sn.ValueType)
    // System.MulticastDelegate
    lazy val DelegateClass: Symbol = getClass(sn.Delegate)
    var Delegate_scalaCallers: List[Symbol] = List()
    // Symbol -> (Symbol, Type): scalaCaller -> (scalaMethodSym, DelegateType)
    // var Delegate_scalaCallerInfos: HashMap[Symbol, (Symbol, Type)] = _
    lazy val Delegate_scalaCallerTargets: HashMap[Symbol, Symbol] = new HashMap()

    def isCorrespondingDelegate(delegateType: Type, functionType: Type): Boolean = {
      isSubType(delegateType, DelegateClass.tpe) &&
      (delegateType.member(nme.apply).tpe match {
      	case MethodType(delegateParams, delegateReturn) =>
      	  isFunctionType(functionType) &&
      	  (functionType.normalize match {
      	    case TypeRef(_, _, args) =>
      	      (delegateParams.map(pt => {
                      if (pt == AnyClass.tpe) definitions.ObjectClass.tpe else pt})
      	       ::: List(delegateReturn)) == args
      	    case _ => false
      	  })
        case _ => false
      })
    }

    // members of class scala.Any
    var Any_==          : Symbol = _
    var Any_!=          : Symbol = _
    var Any_equals      : Symbol = _
    var Any_hashCode    : Symbol = _
    var Any_toString    : Symbol = _
    var Any_isInstanceOf: Symbol = _
    var Any_asInstanceOf: Symbol = _

    // members of class java.lang.{Object, String}
    var Object_eq          : Symbol = _
    var Object_ne          : Symbol = _
    var Object_==          : Symbol = _
    var Object_!=          : Symbol = _
    var Object_synchronized: Symbol = _
    var Object_isInstanceOf: Symbol = _
    var Object_asInstanceOf: Symbol = _
    def Object_getClass  = getMember(ObjectClass, nme.getClass_)
    def Object_clone     = getMember(ObjectClass, nme.clone_)
    def Object_finalize  = getMember(ObjectClass, nme.finalize_)
    def Object_notify    = getMember(ObjectClass, nme.notify_)
    def Object_notifyAll = getMember(ObjectClass, nme.notifyAll_)
    def Object_equals    = getMember(ObjectClass, nme.equals_)
    def Object_hashCode  = getMember(ObjectClass, nme.hashCode_)
    def Object_toString  = getMember(ObjectClass, nme.toString_)

    var String_+           : Symbol = _

    // boxed classes
    lazy val ObjectRefClass         = getClass("scala.runtime.ObjectRef")
    lazy val BoxesRunTimeClass      = getModule("scala.runtime.BoxesRunTime")
    lazy val BoxedArrayClass        = getClass("scala.runtime.BoxedArray")
    lazy val BoxedAnyArrayClass     = getClass("scala.runtime.BoxedAnyArray")
    lazy val BoxedObjectArrayClass  = getClass("scala.runtime.BoxedObjectArray")
    lazy val BoxedNumberClass       = getClass(sn.BoxedNumber)
    lazy val BoxedCharacterClass    = getClass(sn.BoxedCharacter)
    lazy val BoxedBooleanClass      = getClass(sn.BoxedBoolean)
    lazy val BoxedUnitClass         = getClass("scala.runtime.BoxedUnit")
    lazy val BoxedUnitModule        = getModule("scala.runtime.BoxedUnit")
      def BoxedUnit_UNIT = getMember(BoxedUnitModule, "UNIT")

    // special attributes
    lazy val SerializableAttr: Symbol = getClass("scala.serializable")
    lazy val DeprecatedAttr: Symbol = getClass("scala.deprecated")
    lazy val BeanPropertyAttr: Symbol = getClass(sn.BeanProperty)
    lazy val BooleanBeanPropertyAttr: Symbol = getClass(sn.BooleanBeanProperty)

    lazy val AnnotationDefaultAttr: Symbol = {
      val attr = newClass(RootClass, nme.AnnotationDefaultATTR, List(AnnotationClass.typeConstructor))
      // This attribute needs a constructor so that modifiers in parsed Java code make sense
      attr.info.decls enter (attr newConstructor NoPosition setInfo MethodType(Nil, attr.tpe))
      attr
    }

    lazy val NativeAttr: Symbol = getClass("scala.native")
    lazy val VolatileAttr: Symbol = getClass("scala.volatile")

    def getModule(fullname: Name): Symbol = getModuleOrClass(fullname, true)
    def getModule2(name1: Name, name2: Name) = try {
      getModuleOrClass(name1, true)
    } catch {
      case ex1: FatalError =>
        try {
          getModuleOrClass(name2, true)
        } catch {
          case ex2: FatalError => throw ex1
        }
    }

    def getClass(fullname: Name): Symbol = {
      var result = getModuleOrClass(fullname, false)
      while (result.isAliasType) result = result.info.typeSymbol
      result
    }

    def getClass2(name1: Name, name2: Name) = try {
      var result = getModuleOrClass(name1, false)
      if (result.isAliasType) getClass(name2) else result
    } catch {
      case ex1: FatalError =>
        try {
          getModuleOrClass(name2, false)
        } catch {
          case ex2: FatalError => throw ex1
        }
    }

    def getMember(owner: Symbol, name: Name): Symbol = {
      if (owner == NoSymbol) return NoSymbol
      val result = owner.info.nonPrivateMember(name)
      if (result == NoSymbol) {
        throw new FatalError(owner.toString() + " does not have a member " + name)
      }
      result
    }

    private def getModuleOrClass(fullname: Name, module: Boolean): Symbol = {
      if (fullname == nme.NOSYMBOL) return NoSymbol
      var sym:Symbol = RootClass
      var i = 0
      var j = fullname.pos('.', i)
      while (j < fullname.length) {
        sym = sym.info.member(fullname.subName(i, j))
        i = j + 1
        j = fullname.pos('.', i)
      }
      val result =
        if (module) sym.info.member(fullname.subName(i, j)).suchThat(_ hasFlag MODULE)
        else sym.info.member(fullname.subName(i, j).toTypeName)
      if (result == NoSymbol) {
        if (settings.debug.value)
          { Console.println(sym.info); Console.println(sym.info.members) }//debug
        throw new MissingRequirementError((if (module) "object " else "class ") + fullname)
      }
      result
    }

    private def newClass(owner: Symbol, name: Name, parents: List[Type]): Symbol = {
      val clazz = owner.newClass(NoPosition, name.toTypeName)
      clazz.setInfo(ClassInfoType(parents, newClassScope(clazz), clazz))
      owner.info.decls.enter(clazz)
      clazz
    }

    private def newCovariantPolyClass(owner: Symbol, name: Name, parent: Symbol => Type): Symbol = {
      val clazz = newClass(owner, name, List())
      val tparam = newTypeParam(clazz, 0) setFlag COVARIANT
      val p = parent(tparam)
/*      p.typeSymbol.initialize
      println(p.typeSymbol + " flags: " + Flags.flagsToString(p.typeSymbol.flags))
      val parents = /*if (p.typeSymbol.isTrait)
        List(definitions.AnyRefClass.tpe, p)
                    else*/ List(p)
      println("creating " + name + " with parents " + parents) */
      clazz.setInfo(
        PolyType(
          List(tparam),
          ClassInfoType(List(AnyRefClass.tpe, p), newClassScope(clazz), clazz)))
    }

    private def newAlias(owner: Symbol, name: Name, alias: Type): Symbol = {
      val tpsym = owner.newAliasType(NoPosition, name.toTypeName)
      tpsym.setInfo(alias)
      owner.info.decls.enter(tpsym)
      tpsym
    }

    private def newMethod(owner: Symbol, name: Name): Symbol = {
      val msym = owner.newMethod(NoPosition, name.encode)
      owner.info.decls.enter(msym)
      msym
    }

    private def newMethod(owner: Symbol, name: Name, formals: List[Type], restpe: Type): Symbol = {
      val msym = newMethod(owner, name)
      val params = msym.newSyntheticValueParams(formals)
      msym.setInfo(MethodType(params, restpe))
    }

    /** tcon receives the type parameter symbol as argument */
    private def newPolyMethod(owner: Symbol, name: Name, tcon: Symbol => Type): Symbol =
      newPolyMethodCon(owner, name, tparam => msym => tcon(tparam))

    /** tcon receives the type parameter symbol and the method symbol as arguments */
    private def newPolyMethodCon(owner: Symbol, name: Name, tcon: Symbol => Symbol => Type): Symbol = {
      val msym = newMethod(owner, name)
      val tparam = newTypeParam(msym, 0)
      msym.setInfo(PolyType(List(tparam), tcon(tparam)(msym)))
    }

    private def newParameterlessMethod(owner: Symbol, name: Name, restpe: Type) =
      newMethod(owner, name).setInfo(PolyType(List(),restpe))

    private def newTypeParam(owner: Symbol, index: Int): Symbol =
      owner.newTypeParameter(NoPosition, "T" + index)
        .setInfo(mkTypeBounds(NothingClass.typeConstructor, AnyClass.typeConstructor))

    val boxedClass = new HashMap[Symbol, Symbol]
    val unboxMethod = new HashMap[Symbol, Symbol] // Type -> Method
    val boxMethod = new HashMap[Symbol, Symbol] // Type -> Method
    val boxedArrayClass = new HashMap[Symbol, Symbol]

    def isUnbox(m: Symbol) = (m.name == nme.unbox) && cond(m.tpe) {
      case MethodType(_, restpe) => cond(unboxMethod get restpe.typeSymbol) {
        case Some(`m`)  => true
      }
    }

    /** Test whether a method symbol is that of a boxing method. */
    def isBox(m: Symbol) = (boxMethod.values contains m) && cond(m.tpe) {
      case MethodType(List(arg), _) => cond(boxMethod get arg.tpe.typeSymbol) {
        case Some(`m`) => true
      }
    }

    val refClass = new HashMap[Symbol, Symbol]
    val abbrvTag = new HashMap[Symbol, Char]

    private def newValueClass(name: Name, tag: Char): Symbol = {
      val boxedName = sn.Boxed(name)

      val clazz = newClass(ScalaPackageClass, name, anyvalparam) setFlag (ABSTRACT | FINAL)
      boxedClass(clazz) = getClass(boxedName)
      boxedArrayClass(clazz) = getClass("scala.runtime.Boxed" + name + "Array")
      refClass(clazz) = getClass("scala.runtime." + name + "Ref")
      abbrvTag(clazz) = tag

      val module = ScalaPackageClass.newModule(NoPosition, name)
      ScalaPackageClass.info.decls.enter(module)
      val mclass = module.moduleClass
      mclass.setInfo(ClassInfoType(List(), newClassScope(mclass), mclass))
      module.setInfo(mclass.tpe)

      val box = newMethod(mclass, nme.box, List(clazz.typeConstructor),
                          ObjectClass.typeConstructor)
      boxMethod(clazz) = box
      val unbox = newMethod(mclass, nme.unbox, List(ObjectClass.typeConstructor),
                            clazz.typeConstructor)
      unboxMethod(clazz) = unbox

      clazz
    }

    /** Sets-up symbols etc. for value classes, and their boxed versions. This
      * method is called once from within the body of init. */
    private def initValueClasses() {
      val booltype = BooleanClass.typeConstructor
      val boolparam = List(booltype)
      val bytetype = ByteClass.typeConstructor
      val byteparam = List(bytetype)
      val chartype = CharClass.typeConstructor
      val charparam = List(chartype)
      val shorttype = ShortClass.typeConstructor
      val shortparam = List(shorttype)
      val inttype = IntClass.typeConstructor
      val intparam = List(inttype)
      val longtype = LongClass.typeConstructor
      val longparam = List(longtype)

      val floattype = FloatClass.typeConstructor
      val floatparam = List(floattype)
      val doubletype = DoubleClass.typeConstructor
      val doubleparam = List(doubletype)

      val stringtype = StringClass.typeConstructor

      // init scala.Boolean
      newParameterlessMethod(BooleanClass, nme.UNARY_!, booltype)
      List(nme.EQ, nme.NE, nme.ZOR, nme.ZAND, nme.OR, nme.AND, nme.XOR) foreach {
        newMethod(BooleanClass, _, boolparam, booltype)
      }

      def initValueClass(clazz: Symbol, isCardinal: Boolean) {
        assert (clazz ne null)
        val boolBinOps  = List(nme.EQ, nme.NE, nme.LT, nme.LE, nme.GT, nme.GE)
        val otherBinOps = List(nme.ADD, nme.SUB, nme.MUL, nme.DIV, nme.MOD)
        val cardBinOps  = List(nme.OR, nme.AND, nme.XOR)
        val shiftOps    = List(nme.LSL, nme.LSR, nme.ASR)

        def addBinops(params: List[Type], restype: Type, isCardinal: Boolean) = {
          boolBinOps foreach  (x => newMethod(clazz, x, params, booltype))
          otherBinOps foreach (x => newMethod(clazz, x, params, restype))

          if (isCardinal)
            cardBinOps foreach (x => newMethod(clazz, x, params, restype))
        }

        // conversion methods
        newParameterlessMethod(clazz, nme.toByte,   bytetype)
        newParameterlessMethod(clazz, nme.toShort,  shorttype)
        newParameterlessMethod(clazz, nme.toChar,   chartype)
        newParameterlessMethod(clazz, nme.toInt,    inttype)
        newParameterlessMethod(clazz, nme.toLong,   longtype)

        newParameterlessMethod(clazz, nme.toFloat,  floattype)
        newParameterlessMethod(clazz, nme.toDouble, doubletype)

        // def +(s: String): String
        newMethod(clazz, nme.ADD, List(stringtype), stringtype)

        val restype = clazz match {
          case LongClass | FloatClass | DoubleClass => clazz.typeConstructor
          case _                                    => inttype
        }

        // shift operations
        if (isCardinal)
          for (op <- shiftOps ; param <- List(intparam, longparam))
            newMethod(clazz, op, param, restype)

        // unary operations
        newParameterlessMethod(clazz, nme.UNARY_+, restype)
        newParameterlessMethod(clazz, nme.UNARY_-, restype)

        if (isCardinal) {
          newParameterlessMethod(clazz, nme.UNARY_~, restype)
        }

        // binary operations
        List(byteparam, shortparam, charparam, intparam) .
          foreach (x => addBinops(x, restype, isCardinal))

        addBinops(longparam,   (if (isCardinal) longtype else restype),               isCardinal)
        addBinops(floatparam,  (if (clazz eq DoubleClass) doubletype else floattype), false     )
        addBinops(doubleparam, doubletype,                                            false     )
      }

      List(ByteClass, ShortClass, CharClass, IntClass, LongClass) foreach (x => initValueClass(x, true))
      List(FloatClass, DoubleClass)                               foreach (x => initValueClass(x, false))

      def addModuleMethod(clazz: Symbol, name: Name, value: Any) {
        val owner = clazz.linkedClassOfClass
        newParameterlessMethod(owner, name, mkConstantType(Constant(value)))
      }
      addModuleMethod(ByteClass,  "MinValue",  java.lang.Byte.MIN_VALUE)
      addModuleMethod(ByteClass,  "MaxValue",  java.lang.Byte.MAX_VALUE)
      addModuleMethod(ShortClass, "MinValue",  java.lang.Short.MIN_VALUE)
      addModuleMethod(ShortClass, "MaxValue",  java.lang.Short.MAX_VALUE)
      addModuleMethod(CharClass,  "MinValue",  java.lang.Character.MIN_VALUE)
      addModuleMethod(CharClass,  "MaxValue",  java.lang.Character.MAX_VALUE)
      addModuleMethod(IntClass,   "MinValue",  java.lang.Integer.MIN_VALUE)
      addModuleMethod(IntClass,   "MaxValue",  java.lang.Integer.MAX_VALUE)
      addModuleMethod(LongClass,  "MinValue",  java.lang.Long.MIN_VALUE)
      addModuleMethod(LongClass,  "MaxValue",  java.lang.Long.MAX_VALUE)

      addModuleMethod(FloatClass, "MinValue", -java.lang.Float.MAX_VALUE)
      addModuleMethod(FloatClass, "MaxValue",  java.lang.Float.MAX_VALUE)
      addModuleMethod(FloatClass, "Epsilon",   java.lang.Float.MIN_VALUE)
      addModuleMethod(FloatClass, "NaN",       java.lang.Float.NaN)
      addModuleMethod(FloatClass, "PositiveInfinity", java.lang.Float.POSITIVE_INFINITY)
      addModuleMethod(FloatClass, "NegativeInfinity", java.lang.Float.NEGATIVE_INFINITY)

      addModuleMethod(DoubleClass, "MinValue", -java.lang.Double.MAX_VALUE)
      addModuleMethod(DoubleClass, "MaxValue",  java.lang.Double.MAX_VALUE)
      addModuleMethod(DoubleClass, "Epsilon",   java.lang.Double.MIN_VALUE)
      addModuleMethod(DoubleClass, "NaN",       java.lang.Double.NaN)
      addModuleMethod(DoubleClass, "PositiveInfinity", java.lang.Double.POSITIVE_INFINITY)
      addModuleMethod(DoubleClass, "NegativeInfinity", java.lang.Double.NEGATIVE_INFINITY)
    }

    /** Is symbol a value class? */
    def isValueClass(sym: Symbol): Boolean =
      (sym eq UnitClass) || (boxedClass contains sym)

    /** Is symbol a value class? */
    def isNumericValueClass(sym: Symbol): Boolean =
      (sym ne BooleanClass) && (boxedClass contains sym)

    def isValueType(sym: Symbol) =
      isValueClass(sym) || unboxMethod.contains(sym)

    /** Is symbol a value or array class? */
    def isUnboxedClass(sym: Symbol): Boolean =
      isValueType(sym) || sym == ArrayClass

    def signature(tp: Type): String = {
      def erasure(tp: Type): Type = tp match {
        case st: SubType => erasure(st.supertype)
        case RefinedType(parents, _) => erasure(parents.head)
        case _ => tp
      }
      def flatNameString(sym: Symbol, separator: Char): String =
        if (sym.owner.isPackageClass) sym.fullNameString('.') + (if (sym.isModuleClass) "$" else "")
        else flatNameString(sym.owner, separator) + "$" + sym.simpleName;
      def signature1(etp: Type): String = {
        if (etp.typeSymbol == ArrayClass) "[" + signature1(erasure(etp.normalize.typeArgs.head))
        else if (isValueClass(etp.typeSymbol)) abbrvTag(etp.typeSymbol).toString()
        else "L" + flatNameString(etp.typeSymbol, '/') + ";"
      }
      val etp = erasure(tp)
      if (etp.typeSymbol == ArrayClass) signature1(etp)
      else flatNameString(etp.typeSymbol, '.')
    }

    private var isInitialized = false

    def init {
      if (isInitialized) return
      isInitialized = true

      EmptyPackageClass.setInfo(ClassInfoType(List(), newClassScope(EmptyPackageClass), EmptyPackageClass))
      EmptyPackage.setInfo(EmptyPackageClass.tpe)
      RootClass.info.decls.enter(EmptyPackage)
      RootClass.info.decls.enter(RootPackage)

      abbrvTag(UnitClass) = 'V'

      initValueClasses()
      val booltype = BooleanClass.typeConstructor

      // members of class scala.Any
      Any_== = newMethod(AnyClass, nme.EQ, anyparam, booltype) setFlag FINAL
      Any_!= = newMethod(AnyClass, nme.NE, anyparam, booltype) setFlag FINAL
      Any_equals = newMethod(AnyClass, nme.equals_, anyparam, booltype)
      Any_hashCode = newMethod(
        AnyClass, nme.hashCode_, List(), IntClass.typeConstructor)
      Any_toString = newMethod(
        AnyClass, nme.toString_, List(), StringClass.typeConstructor)

      Any_isInstanceOf = newPolyMethod(
        AnyClass, nme.isInstanceOf_, tparam => booltype) setFlag FINAL
      Any_asInstanceOf = newPolyMethod(
        AnyClass, nme.asInstanceOf_, tparam => tparam.typeConstructor) setFlag FINAL

      // members of class java.lang.{Object, String}
      Object_== = newMethod(ObjectClass, nme.EQ, anyrefparam, booltype) setFlag FINAL
      Object_!= = newMethod(ObjectClass, nme.NE, anyrefparam, booltype) setFlag FINAL
      Object_eq = newMethod(ObjectClass, nme.eq, anyrefparam, booltype) setFlag FINAL
      Object_ne = newMethod(ObjectClass, "ne", anyrefparam, booltype) setFlag FINAL
      Object_synchronized = newPolyMethodCon(
        ObjectClass, nme.synchronized_,
        tparam => msym => MethodType(msym.newSyntheticValueParams(List(tparam.typeConstructor)), tparam.typeConstructor)) setFlag FINAL
      Object_isInstanceOf = newPolyMethod(
        ObjectClass, "$isInstanceOf",
        tparam => MethodType(List(), booltype)) setFlag FINAL
      Object_asInstanceOf = newPolyMethod(
        ObjectClass, "$asInstanceOf",
        tparam => MethodType(List(), tparam.typeConstructor)) setFlag FINAL
      String_+ = newMethod(
        StringClass, "+", anyparam, StringClass.typeConstructor) setFlag FINAL

      // #2264
      val tmp = AnnotationDefaultAttr

      if (forMSIL) {
        val intType = IntClass.typeConstructor
        val intParam = List(intType)
        val longType = LongClass.typeConstructor
        val charType = CharClass.typeConstructor
        val unitType = UnitClass.typeConstructor
        val stringType = StringClass.typeConstructor
        val stringParam = List(stringType)

        // additional methods of Object
        newMethod(ObjectClass, "clone", List(), AnyRefClass.typeConstructor)
        newMethod(ObjectClass, "wait", List(), unitType)
        newMethod(ObjectClass, "wait", List(longType), unitType)
        newMethod(ObjectClass, "wait", List(longType, intType), unitType)
        newMethod(ObjectClass, "notify", List(), unitType)
        newMethod(ObjectClass, "notifyAll", List(), unitType)

        // additional methods of String
        newMethod(StringClass, "length", List(), intType)
        newMethod(StringClass, "compareTo", stringParam, intType)
        newMethod(StringClass, "charAt", intParam, charType)
        newMethod(StringClass, "concat", stringParam, stringType)
        newMethod(StringClass, "indexOf", intParam, intType)
        newMethod(StringClass, "indexOf", List(intType, intType), intType)
        newMethod(StringClass, "indexOf", stringParam, intType)
        newMethod(StringClass, "indexOf", List(stringType, intType), intType)
        newMethod(StringClass, "lastIndexOf", intParam, intType)
        newMethod(StringClass, "lastIndexOf", List(intType, intType), intType)
        newMethod(StringClass, "lastIndexOf", stringParam, intType)
        newMethod(StringClass, "lastIndexOf", List(stringType, intType), intType)
        newMethod(StringClass, "toLowerCase", List(), stringType)
        newMethod(StringClass, "toUpperCase", List(), stringType)
        newMethod(StringClass, "startsWith", stringParam, booltype)
        newMethod(StringClass, "endsWith", stringParam, booltype)
        newMethod(StringClass, "substring", intParam, stringType)
        newMethod(StringClass, "substring", List(intType, intType), stringType)
        newMethod(StringClass, "trim", List(), stringType)
        newMethod(StringClass, "intern", List(), stringType)
        newMethod(StringClass, "replace", List(charType, charType), stringType)
        newMethod(StringClass, "toCharArray", List(),
                  appliedType(ArrayClass.typeConstructor, List(charType)))
      }
    } //init

    var nbScalaCallers: Int = 0
    def newScalaCaller(delegateType: Type): Symbol = {
      assert(forMSIL, "scalaCallers can only be created if target is .NET")
      // object: reference to object on which to call (scala-)metod
      val paramTypes: List[Type] = List(ObjectClass.tpe)
      val name: String =  "$scalaCaller$$" + nbScalaCallers
      // tparam => resultType, which is the resultType of PolyType, i.e. the result type after applying the
      // type parameter =-> a MethodType in this case
      // TODO: set type bounds manually (-> MulticastDelegate), see newTypeParam
      val newCaller = newMethod(DelegateClass, name, paramTypes, delegateType) setFlag (FINAL | STATIC)
      // val newCaller = newPolyMethod(DelegateClass, name,
      // tparam => MethodType(paramTypes, tparam.typeConstructor)) setFlag (FINAL | STATIC)
      Delegate_scalaCallers = Delegate_scalaCallers ::: List(newCaller)
      nbScalaCallers += 1
      newCaller
    }

    // def addScalaCallerInfo(scalaCaller: Symbol, methSym: Symbol, delType: Type) {
    // assert(Delegate_scalaCallers contains scalaCaller)
    // Delegate_scalaCallerInfos += (scalaCaller -> (methSym, delType))
    // }

    def addScalaCallerInfo(scalaCaller: Symbol, methSym: Symbol) {
      assert(Delegate_scalaCallers contains scalaCaller)
      Delegate_scalaCallerTargets += (scalaCaller -> methSym)
    }
  }
}
