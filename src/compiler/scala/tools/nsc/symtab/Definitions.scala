/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.symtab

import scala.collection.mutable.{HashMap, HashSet}
import scala.tools.nsc.util.{Position, NoPosition}
import Flags._

trait Definitions {
  self: SymbolTable =>

  object definitions {
    def isDefinitionsInitialized = isInitialized

    // root packages and classes
    lazy val RootPackage: Symbol = {
      val rp=NoSymbol.newValue(NoPosition, nme.ROOTPKG)
        .setFlag(FINAL | MODULE | PACKAGE | JAVA)
        .setInfo(PolyType(List(), RootClass.tpe))
      RootClass.setSourceModule(rp)
      rp
    }
    lazy val RootClass: ModuleClassSymbol = NoSymbol.newModuleClass(NoPosition, nme.ROOT.toTypeName)
          .setFlag(FINAL | MODULE | PACKAGE | JAVA).setInfo(rootLoader)

    lazy val EmptyPackage: Symbol = RootClass.newPackage(NoPosition, nme.EMPTY_PACKAGE_NAME).setFlag(FINAL)

    lazy val EmptyPackageClass: Symbol = EmptyPackage.moduleClass

    var emptypackagescope: Scope = null //debug

    lazy val JavaLangPackage: Symbol = getModule(sn.JavaLang)
    lazy val ScalaPackage: Symbol = getModule("scala")
    lazy val ScalaPackageClass: Symbol = ScalaPackage.tpe.typeSymbol

    lazy val ScalaCollectionImmutablePackage: Symbol = getModule("scala.collection.immutable")
    lazy val ScalaCollectionImmutablePackageClass: Symbol = ScalaCollectionImmutablePackage.tpe.typeSymbol

    var AnyClass: Symbol = _
    var AnyValClass: Symbol = _
    var AnyRefClass: Symbol = _
    lazy val ObjectClass: Symbol = getClass(sn.Object)

    lazy val anyrefparam = List(AnyRefClass.typeConstructor)

    var NullClass: Symbol = _
    var NothingClass: Symbol = _

    lazy val RuntimeNothingClass = getClass("scala.runtime.Nothing$")
    lazy val RuntimeNullClass = getClass("scala.runtime.Null$")

    var SingletonClass: Symbol = _
    lazy val uncheckedStableClass = getClass("scala.annotation.unchecked.uncheckedStable")
    lazy val uncheckedVarianceClass = getClass("scala.annotation.unchecked.uncheckedVariance")

    lazy val ClassClass: Symbol = getClass(sn.Class)
      def Class_getMethod = getMember(ClassClass, nme.getMethod_)
    lazy val StringClass: Symbol = getClass(sn.String)
    lazy val ThrowableClass: Symbol = getClass(sn.Throwable)
    lazy val NullPointerExceptionClass: Symbol = getClass(sn.NPException)
    lazy val NonLocalReturnExceptionClass: Symbol = getClass(sn.NLRException)
    lazy val InvocationTargetExceptionClass: Symbol =
      getClass("java.lang.reflect.InvocationTargetException") // java is hard coded because only used by structural values

    // System.ValueType
    lazy val ValueTypeClass: Symbol = getClass(sn.ValueType)
    // System.MulticastDelegate
    lazy val DelegateClass: Symbol = getClass(sn.Delegate)
    var Delegate_scalaCallers: List[Symbol] = List()
    // Symbol -> (Symbol, Type): scalaCaller -> (scalaMethodSym, DelegateType)
    // var Delegate_scalaCallerInfos: HashMap[Symbol, (Symbol, Type)] = _
    lazy val Delegate_scalaCallerTargets: HashMap[Symbol, Symbol] = new HashMap()

    // the scala value classes
    var UnitClass: Symbol = _
    var BooleanClass: Symbol = _
      def Boolean_not = getMember(BooleanClass, nme.UNARY_!)
      def Boolean_and = getMember(BooleanClass, nme.ZAND)
      def Boolean_or  = getMember(BooleanClass, nme.ZOR)
    var ByteClass: Symbol = _
    var ShortClass: Symbol = _
    var CharClass: Symbol = _
    var IntClass: Symbol = _
      def Int_Or  = definitions.getMember(definitions.IntClass, nme.OR)
      def Int_And = definitions.getMember(definitions.IntClass, nme.AND)
      def Int_==  = definitions.getMember(definitions.IntClass, nme.EQ)
      def Int_!=  = definitions.getMember(definitions.IntClass, nme.NE)

    var LongClass: Symbol = _
    var FloatClass: Symbol = _
    var DoubleClass: Symbol = _

    // the scala reference classes
    lazy val ScalaObjectClass: Symbol = getClass("scala.ScalaObject")
    lazy val AnnotationClass: Symbol = getClass("scala.Annotation")
    lazy val ClassfileAnnotationClass: Symbol = getClass("scala.ClassfileAnnotation")
    lazy val StaticAnnotationClass: Symbol = getClass("scala.StaticAnnotation")
    lazy val TypeConstraintClass: Symbol = getClass("scala.TypeConstraint")
    lazy val ManifestClass: Symbol = getClass("scala.reflect.Manifest")
    lazy val ManifestModule: Symbol = getModule("scala.reflect.Manifest")
    lazy val OptManifestClass: Symbol = getClass("scala.reflect.OptManifest")
    lazy val NoManifest: Symbol = getModule("scala.reflect.NoManifest")

    var CodeClass: Symbol = _
    var CodeModule: Symbol = _
      def Code_lift = getMember(CodeModule, nme.lift_)
    lazy val PartialFunctionClass: Symbol = getClass("scala.PartialFunction")
    lazy val IterableClass: Symbol = getClass2("scala.Iterable", "scala.collection.Iterable")
      def Iterable_next = getMember(IterableClass, nme.next)
      def Iterable_hasNext = getMember(IterableClass, nme.hasNext)
    lazy val IteratorClass: Symbol = getClass2("scala.Iterator", "scala.collection.Iterator")
    lazy val SeqClass: Symbol = getClass2("scala.Seq", "scala.collection.Sequence")
    lazy val SeqModule: Symbol = getModule2("scala.Seq", "scala.collection.Sequence")
    lazy val TraversableClass: Symbol = getClass("scala.collection.Traversable")
    lazy val RandomAccessSeqMutableClass: Symbol = getMember(
      getModule2("scala.RandomAccessSeq", "scala.collection.Vector"), nme.Mutable)
    def Seq_length = getMember(SeqClass, nme.length)

    lazy val ListClass: Symbol = getClass2("scala.List", "scala.collection.immutable.List")
      def List_isEmpty = getMember(ListClass, nme.isEmpty)
      def List_head = getMember(ListClass, nme.head)
      def List_tail = getMember(ListClass, nme.tail)
    lazy val ListModule: Symbol = getModule2("scala.List", "scala.collection.immutable.List")
      def List_apply = getMember(ListModule, nme.apply)
    lazy val ArrayClass: Symbol = getClass("scala.Array")
      def Array_apply = getMember(ArrayClass, nme.apply)
      def Array_update = getMember(ArrayClass, nme.update)
    lazy val ArrayModule: Symbol = getModule("scala.Array")
      def ArrayModule_apply = getMember(ArrayModule, nme.apply)
    lazy val SerializableClass: Symbol = getClass(sn.Serializable)
    lazy val MethodClass: Symbol = getClass(sn.MethodAsObject)
    lazy val MethodCacheClass: Symbol = getClass("scala.runtime.MethodCache")
      def methodCache_find = getMember(MethodCacheClass, nme.find_)
      def methodCache_add = getMember(MethodCacheClass, nme.add_)
    lazy val EmptyMethodCacheClass: Symbol = getClass("scala.runtime.EmptyMethodCache")

    // invoke dynamic support
    lazy val LinkageModule: Symbol = getModule("java.dyn.Linkage")
      lazy val Linkage_invalidateCallerClass = getMember(LinkageModule, "invalidateCallerClass")
    lazy val DynamicDispatchClass: Symbol = getModule("scala.runtime.DynamicDispatch")
      lazy val DynamicDispatch_DontSetTarget: Symbol = getMember(DynamicDispatchClass, "DontSetTarget")

    lazy val PredefModule: Symbol = getModule("scala.Predef")
      def Predef_classOf = getMember(PredefModule, nme.classOf)
      def Predef_classOfType(classType: Type): Type =
        if (!ClassClass.unsafeTypeParams.isEmpty && !phase.erasedTypes)
          appliedType(ClassClass.tpe, List(classType))
        else ClassClass.tpe
      def Predef_identity = getMember(PredefModule, nme.identity)
      def Predef_error    = getMember(PredefModule, nme.error)
    lazy val ConsoleModule: Symbol = getModule("scala.Console")
    lazy val MatchErrorClass: Symbol = getClass("scala.MatchError")
    lazy val UninitializedErrorClass: Symbol = getClass("scala.UninitializedFieldError")
    //var MatchErrorModule: Symbol = _
    //  def MatchError_fail = getMember(MatchErrorModule, nme.fail)
    //  def MatchError_report = getMember(MatchErrorModule, nme.report)
    lazy val IndexOutOfBoundsExceptionClass: Symbol = getClass(sn.IOOBException)
    lazy val ScalaRunTimeModule: Symbol = getModule("scala.runtime.ScalaRunTime")
      def SeqFactory = getMember(ScalaRunTimeModule, nme.Seq);
      def checkDefinedMethod = getMember(ScalaRunTimeModule, "checkDefined")
      def isArrayMethod = getMember(ScalaRunTimeModule, "isArray")
    lazy val NotNullClass: Symbol = getClass("scala.NotNull")
    var RepeatedParamClass: Symbol = _
    var ByNameParamClass: Symbol = _
    //var UnsealedClass: Symbol = _
    lazy val UncheckedClass: Symbol = getClass("scala.unchecked")
    lazy val TailrecClass: Symbol = getClass("scala.annotation.tailrec")
    lazy val SwitchClass: Symbol = getClass("scala.annotation.switch")
    lazy val ExperimentalClass: Symbol = getClass("scala.annotation.experimental")

    var EqualsPatternClass: Symbol = _

    val MaxTupleArity = 22
    val TupleClass: Array[Symbol] = new Array(MaxTupleArity + 1)
      def tupleField(n: Int, j: Int) = getMember(TupleClass(n), "_" + j)
      def isTupleType(tp: Type): Boolean = tp.normalize match {
        case TypeRef(_, sym, elems) =>
          elems.length <= MaxTupleArity && sym == TupleClass(elems.length)
        case _ =>
          false
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
    val MaxProductArity = 22
    /* <unapply> */
    val ProductClass: Array[Symbol] = new Array(MaxProductArity + 1)
      def productProj(z:Symbol, j: Int): Symbol = getMember(z, nme.Product_(j))
      def productProj(n: Int,   j: Int): Symbol = productProj(ProductClass(n), j)
    /** returns true if this type is exactly ProductN[T1,...,Tn], not some subclass */
      def isExactProductType(tp: Type): Boolean = tp.normalize match {
        case TypeRef(_, sym, elems) =>
          elems.length <= MaxProductArity && sym == ProductClass(elems.length)
        case _ =>
          false
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
      tpe.baseClasses.find { x => definitions.isExactProductType(x.tpe) } match {
        case Some(p) => Some(tpe.baseType(p).typeArgs)
        case _       => None
      }

    lazy val OptionClass: Symbol = getClass("scala.Option")

    lazy val SomeClass: Symbol = getClass("scala.Some")
    lazy val NoneClass: Symbol = getModule("scala.None")

    def isOptionType(tp: Type) = tp.normalize match {
      case TypeRef(_, sym, List(_)) if sym == OptionClass => true
      case _ => false
    }
    def isOptionOrSomeType(tp: Type) = tp.normalize match {
      case TypeRef(_, sym, List(_)) => sym == OptionClass || sym == SomeClass
      case _ => false
    }
    def optionType(tp: Type) =
      typeRef(OptionClass.typeConstructor.prefix, OptionClass, List(tp))

    def isSomeType(tp: Type) = tp.normalize match {
      case TypeRef(_, sym, List(_)) if sym == SomeClass => true
      case _ => false
    }
    def someType(tp: Type) =
      typeRef(SomeClass.typeConstructor.prefix, SomeClass, List(tp))

    def isNoneType(tp: Type) = tp.normalize match {
      case TypeRef(_, sym, List(_)) if sym == NoneClass => true
      case _ => false
    }

    def unapplyUnwrap(tpe:Type) = (tpe match {
      case PolyType(_,MethodType(_, res)) => res
      case MethodType(_, res)             => res
      case tpe                            => tpe
    }).normalize

    /* </unapply> */
    val MaxFunctionArity = 22
    val FunctionClass: Array[Symbol] = new Array(MaxFunctionArity + 1)
      def functionApply(n: Int) = getMember(FunctionClass(n), nme.apply)
      def functionType(formals: List[Type], restpe: Type) =
        if (formals.length <= MaxFunctionArity) {
          val sym = FunctionClass(formals.length)
          typeRef(sym.typeConstructor.prefix, sym, formals ::: List(restpe))
        } else NoType;
      def isFunctionType(tp: Type): Boolean = tp.normalize match {
        case TypeRef(_, sym, args) =>
          (args.length > 0) && (args.length - 1 <= MaxFunctionArity) &&
          (sym == FunctionClass(args.length - 1))
        case _ =>
          false
      }

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

    def seqType(arg: Type) =
      typeRef(SeqClass.typeConstructor.prefix, SeqClass, List(arg))

    lazy val NilModule: Symbol = getModule2("scala.Nil", "scala.collection.immutable.Nil")
    lazy val ConsClass: Symbol = getClass2("scala.$colon$colon", "scala.collection.immutable.$colon$colon")

    // members of class scala.Any
    var Any_==          : Symbol = _
    var Any_!=          : Symbol = _
    var Any_equals      : Symbol = _
    var Any_hashCode    : Symbol = _
    var Any_toString    : Symbol = _
    var Any_isInstanceOf: Symbol = _
    var Any_asInstanceOf: Symbol = _
    var Any_isInstanceOfErased: Symbol = _
    var Any_asInstanceOfErased: Symbol = _

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

    // members of class scala.Iterator
    var Iterator_next      : Symbol = _
    var Iterator_hasNext   : Symbol = _

    // pattern wildcard
    var PatternWildcard: Symbol = _

    // boxed classes
    lazy val BoxesRunTimeClass = getModule("scala.runtime.BoxesRunTime")
    lazy val BoxedArrayClass = getClass("scala.runtime.BoxedArray")
    lazy val BoxedAnyArrayClass = getClass("scala.runtime.BoxedAnyArray")
    lazy val BoxedObjectArrayClass = getClass("scala.runtime.BoxedObjectArray")
    lazy val BoxedUnitClass = getClass("scala.runtime.BoxedUnit")
    lazy val BoxedNumberClass = getClass(sn.BoxedNumber)
    lazy val BoxedCharacterClass = getClass(sn.BoxedCharacter)
    lazy val BoxedBooleanClass = getClass(sn.BoxedBoolean)
    lazy val BoxedUnitModule = getModule("scala.runtime.BoxedUnit")
      def BoxedUnit_UNIT = getMember(BoxedUnitModule, "UNIT")
    lazy val ObjectRefClass = getClass("scala.runtime.ObjectRef")

    // special attributes
    lazy val SerializableAttr: Symbol = getClass("scala.serializable")
    lazy val DeprecatedAttr: Symbol = getClass("scala.deprecated")
    lazy val BeanPropertyAttr: Symbol = getClass(sn.BeanProperty)
    lazy val BooleanBeanPropertyAttr: Symbol = getClass(sn.BooleanBeanProperty)
    var AnnotationDefaultAttr: Symbol = _
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

    def isUnbox(m: Symbol) = m.name == nme.unbox && {
      m.tpe match {
        case MethodType(_, restpe) => (unboxMethod get restpe.typeSymbol) match {
          case Some(`m`) => true
          case _ => false
        }
        case _ => false
      }
    }

    /** Test whether a method symbol is that of a boxing method. */
    def isBox(m: Symbol) = (boxMethod.values contains m) && {
      m.tpe match {
        case MethodType(List(arg), _) => (boxMethod get arg.tpe.typeSymbol) match {
          case Some(`m`) => true
          case _ => false
        }
        case _ => false
      }
    }

    val refClass = new HashMap[Symbol, Symbol]
    private val abbrvTag = new HashMap[Symbol, Char]

    private def newValueClass(name: Name, tag: Char): Symbol = {
      val boxedName = sn.Boxed(name)

      val clazz =
        newClass(ScalaPackageClass, name, List(AnyValClass.typeConstructor))
        .setFlag(ABSTRACT | FINAL)
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
      newMethod(BooleanClass, nme.EQ,   boolparam, booltype)
      newMethod(BooleanClass, nme.NE,   boolparam, booltype)
      newMethod(BooleanClass, nme.ZOR,  boolparam, booltype)
      newMethod(BooleanClass, nme.ZAND, boolparam, booltype)
      newMethod(BooleanClass, nme.OR,   boolparam, booltype)
      newMethod(BooleanClass, nme.AND,  boolparam, booltype)
      newMethod(BooleanClass, nme.XOR,  boolparam, booltype)

      def initValueClass(clazz: Symbol, isCardinal: Boolean) {
        assert (clazz ne null)

        def addBinops(params: List[Type], restype: Type, isCardinal: Boolean) = {
          newMethod(clazz, nme.EQ,  params, booltype)
          newMethod(clazz, nme.NE,  params, booltype)
          newMethod(clazz, nme.LT,  params, booltype)
          newMethod(clazz, nme.LE,  params, booltype)
          newMethod(clazz, nme.GT,  params, booltype)
          newMethod(clazz, nme.GE,  params, booltype)
          newMethod(clazz, nme.ADD, params, restype)
          newMethod(clazz, nme.SUB, params, restype)
          newMethod(clazz, nme.MUL, params, restype)
          newMethod(clazz, nme.DIV, params, restype)
          newMethod(clazz, nme.MOD, params, restype)
          if (isCardinal) {
            newMethod(clazz, nme.OR, params, restype)
            newMethod(clazz, nme.AND, params, restype)
            newMethod(clazz, nme.XOR, params, restype)
          }
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

        val restype =
          if ((clazz eq LongClass) ||
              (clazz eq FloatClass) ||
              (clazz eq DoubleClass))
            clazz.typeConstructor
          else inttype

        // shift operations
        if (isCardinal) {
          newMethod(clazz, nme.LSL, intparam,  restype)
          newMethod(clazz, nme.LSL, longparam, restype)
          newMethod(clazz, nme.LSR, intparam,  restype)
          newMethod(clazz, nme.LSR, longparam, restype)
          newMethod(clazz, nme.ASR, intparam,  restype)
          newMethod(clazz, nme.ASR, longparam, restype)
        }

        // unary operations
        newParameterlessMethod(clazz, nme.UNARY_+, restype)
        newParameterlessMethod(clazz, nme.UNARY_-, restype)

        if (isCardinal) {
          newParameterlessMethod(clazz, nme.UNARY_~, restype)
        }

        // binary operations
        val restype2 = if (isCardinal) longtype else restype
        addBinops(byteparam,   restype,    isCardinal)
        addBinops(shortparam,  restype,    isCardinal)
        addBinops(charparam,   restype,    isCardinal)
        addBinops(intparam,    restype,    isCardinal)
        addBinops(longparam,   restype2,   isCardinal)

        val restype3 = if (clazz eq DoubleClass) doubletype else floattype
        addBinops(floatparam,  restype3,   false)
        addBinops(doubleparam, doubletype, false)
      }

      initValueClass(ByteClass,   true)
      initValueClass(ShortClass,  true)
      initValueClass(CharClass,   true)
      initValueClass(IntClass,    true)
      initValueClass(LongClass,   true)
      initValueClass(FloatClass,  false)
      initValueClass(DoubleClass, false)

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

      AnyClass = newClass(ScalaPackageClass, nme.Any, List()).setFlag(ABSTRACT)
      val any = List(AnyClass.typeConstructor)

      AnyValClass = newClass(ScalaPackageClass, nme.AnyVal, any)
        .setFlag(FINAL | SEALED)
      AnyRefClass =
        newAlias(ScalaPackageClass, nme.AnyRef, ObjectClass.typeConstructor)

      NullClass = newClass(ScalaPackageClass, nme.Null, anyrefparam)
        .setFlag(ABSTRACT | TRAIT | FINAL)

      NothingClass = newClass(ScalaPackageClass, nme.Nothing, any)
        .setFlag(ABSTRACT | TRAIT | FINAL)

      SingletonClass = newClass(ScalaPackageClass, nme.Singleton, any)
        .setFlag(ABSTRACT | TRAIT | FINAL)

      UnitClass =
        newClass(ScalaPackageClass, nme.Unit, List(AnyValClass.typeConstructor))
        .setFlag(ABSTRACT | FINAL)
      abbrvTag(UnitClass) = 'V'

      BooleanClass = newValueClass(nme.Boolean, 'Z')
      ByteClass =    newValueClass(nme.Byte, 'B')
      ShortClass =   newValueClass(nme.Short, 'S')
      CharClass =    newValueClass(nme.Char, 'C')
      IntClass =     newValueClass(nme.Int, 'I')
      LongClass =    newValueClass(nme.Long, 'L')
      FloatClass =   newValueClass(nme.Float, 'F')
      DoubleClass =  newValueClass(nme.Double, 'D')

      CodeClass = getClass(sn.Code)
      CodeModule = getModule(sn.Code)
      RepeatedParamClass = newCovariantPolyClass(
        ScalaPackageClass, nme.REPEATED_PARAM_CLASS_NAME,
        tparam => seqType(tparam.typeConstructor))
      ByNameParamClass = newCovariantPolyClass(
        ScalaPackageClass, nme.BYNAME_PARAM_CLASS_NAME, tparam => AnyClass.typeConstructor)

      EqualsPatternClass = newClass(ScalaPackageClass, nme.EQUALS_PATTERN_NAME, List());
      {
        val tparam = newTypeParam(EqualsPatternClass, 0)
        EqualsPatternClass.setInfo(
          PolyType(
            List(tparam),
            ClassInfoType(List(AnyClass.typeConstructor), newClassScope(EqualsPatternClass), EqualsPatternClass)))
      }

      /* <unapply> */
      //UnsealedClass = getClass("scala.unsealed") //todo: remove once 2.4 is out.

      for (i <- 1 to MaxTupleArity) {
        TupleClass(i)   = getClass(  "scala.Tuple" + i)
      }
      for (i <- 1 to MaxProductArity) {
        ProductClass(i) = getClass("scala.Product" + i)
      }
      /* </unapply> */
      for (i <- 0 to MaxFunctionArity) {
        FunctionClass(i) = getClass("scala.Function" + i)
      }
      initValueClasses()
      val booltype = BooleanClass.typeConstructor

      // members of class scala.Any
      Any_== = newMethod(AnyClass, nme.EQ, any, booltype) setFlag FINAL
      Any_!= = newMethod(AnyClass, nme.NE, any, booltype) setFlag FINAL
      Any_equals = newMethod(AnyClass, nme.equals_, any, booltype)
      Any_hashCode = newMethod(
        AnyClass, nme.hashCode_, List(), IntClass.typeConstructor)
      Any_toString = newMethod(
        AnyClass, nme.toString_, List(), StringClass.typeConstructor)

      Any_isInstanceOf = newPolyMethod(
        AnyClass, nme.isInstanceOf_, tparam => booltype) setFlag FINAL
      Any_asInstanceOf = newPolyMethod(
        AnyClass, nme.asInstanceOf_, tparam => tparam.typeConstructor) setFlag FINAL
      Any_isInstanceOfErased = newPolyMethod(
        AnyClass, nme.isInstanceOfErased, tparam => booltype) setFlag FINAL
      //todo: do we need this?
      Any_asInstanceOfErased = newPolyMethod(
        AnyClass, nme.asInstanceOfErased, tparam => tparam.typeConstructor) setFlag FINAL

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
        StringClass, "+", any, StringClass.typeConstructor) setFlag FINAL

      PatternWildcard = NoSymbol.newValue(NoPosition, "_").setInfo(NothingClass.typeConstructor)

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

      AnnotationDefaultAttr = newClass(RootClass,
                                       nme.AnnotationDefaultATTR,
                                       List(AnnotationClass.typeConstructor))
      // This attribute needs a constructor so that modifiers in parsed
      // Java code make sense
      AnnotationDefaultAttr.info.decls.enter(
        AnnotationDefaultAttr.newConstructor(NoPosition)
          .setInfo(MethodType(List(), AnnotationDefaultAttr.tpe)))
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
