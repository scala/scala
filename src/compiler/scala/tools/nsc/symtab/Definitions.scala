/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.symtab

import scala.collection.mutable.{HashMap, HashSet}
import scala.tools.nsc.util.Position
import Flags._

trait Definitions requires SymbolTable {

  object definitions {
    def isDefinitionsInitialized = isInitialized

    // root packages and classes
    var RootPackage: Symbol = _
    var RootClass: Symbol = _
    var EmptyPackage: Symbol = _
    var EmptyPackageClass: Symbol = _
    var emptypackagescope: Scope = null //debug

    var JavaLangPackage: Symbol = _
    var ScalaPackage: Symbol = _
    var ScalaPackageClass: Symbol = _

    var AnyClass: Symbol = _
    var AnyValClass: Symbol = _
    var ObjectClass: Symbol = _

    var AnyRefClass: Symbol = _

    var AllRefClass: Symbol = _
    var AllClass: Symbol = _

    var ClassClass: Symbol = _
    var StringClass: Symbol = _
    var ThrowableClass: Symbol = _
    var NullPointerExceptionClass: Symbol = _
    var NonLocalReturnExceptionClass: Symbol = _

    // the scala value classes
    var UnitClass: Symbol = _
    var BooleanClass: Symbol = _
      def Boolean_not = getMember(BooleanClass, nme.ZNOT)
      def Boolean_and = getMember(BooleanClass, nme.ZAND)
      def Boolean_or  = getMember(BooleanClass, nme.ZOR)
    var ByteClass: Symbol = _
    var ShortClass: Symbol = _
    var CharClass: Symbol = _
    var IntClass: Symbol = _
    var LongClass: Symbol = _
    var FloatClass: Symbol = _
    var DoubleClass: Symbol = _

    // the scala reference classes
    var ScalaObjectClass: Symbol = _
      def ScalaObjectClass_tag = getMember(ScalaObjectClass, nme.tag)
    var AttributeClass: Symbol = _
    //var ChannelClass: Symbol = _
    //  def Channel_send = getMember(ChannelClass, nme.send)
    //  def Channel_receive = getMember(ChannelClass, nme.receive)
    //var RemoteRefClass: Symbol = _
    var CodeClass: Symbol = _
    var CodeModule: Symbol = _
    var PartialFunctionClass: Symbol = _
    var ByNameFunctionClass: Symbol = _
    var IterableClass: Symbol = _
      def Iterable_next = getMember(IterableClass, nme.next)
      def Iterable_hasNext = getMember(IterableClass, nme.hasNext)
    var IteratorClass: Symbol = _
    var SeqClass: Symbol = _
      def Seq_length = getMember(SeqClass, nme.length)
    var ListClass: Symbol = _
      def List_isEmpty = getMember(ListClass, nme.isEmpty)
      def List_head = getMember(ListClass, nme.head)
      def List_tail = getMember(ListClass, nme.tail)
    var ListModule: Symbol = _
      def List_apply = getMember(ListModule, nme.apply)
    var ArrayClass: Symbol = _
    var SerializableClass: Symbol = _
    var PredefModule: Symbol = _
      def Predef_classOf = getMember(PredefModule, nme.classOf)
      def Predef_identity = getMember(PredefModule, nme.identity)
    var ConsoleModule: Symbol = _
    var MatchErrorClass: Symbol = _
    var MatchErrorModule: Symbol = _
      def MatchError_fail = getMember(MatchErrorModule, nme.fail)
      def MatchError_report = getMember(MatchErrorModule, nme.report)
    var IndexOutOfBoundsExceptionClass: Symbol = _
    //var RemoteExecutionModule: Symbol = _
    //  def RemoteExecution_detach = getMember(RemoteExecutionModule, "detach")
    var ScalaRunTimeModule: Symbol = _
      def SeqFactory = getMember(ScalaRunTimeModule, nme.Seq);
      def checkDefinedMethod = getMember(ScalaRunTimeModule, "checkDefined")
    var RepeatedParamClass: Symbol = _
    var ByNameParamClass: Symbol = _

    val MaxTupleArity = 9
    val TupleClass: Array[Symbol] = new Array(MaxTupleArity + 1)
      def tupleField(n: Int, j: Int) = getMember(TupleClass(n), "_" + j)
      def isTupleType(tp: Type): Boolean = tp match {
        case TypeRef(_, sym, elems) =>
          elems.length <= MaxTupleArity && sym == TupleClass(elems.length);
        case _ =>
          false
      }
      def tupleType(elems: List[Type]) =
        if (elems.length <= MaxTupleArity) {
          val sym = TupleClass(elems.length)
          typeRef(sym.typeConstructor.prefix, sym, elems)
        } else NoType;

    val MaxProductArity = 22
    /* <unapply> */
    val ProductClass: Array[Symbol] = new Array(MaxProductArity + 1)
      def productProj(n: Int, j: Int) = getMember(ProductClass(n), "_" + j)
      def isProductType(tp: Type): Boolean = tp match {
        case TypeRef(_, sym, elems) =>
          elems.length <= MaxProductArity && sym == ProductClass(elems.length);
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

    var OptionClass: Symbol = _
    def SomeClass: Symbol = getClass("scala.Some")
    def NoneClass: Symbol = getModule("scala.None")

    def isOptionType(tp: Type) = tp match {
      case TypeRef(_, sym, List(_)) if sym == OptionClass => true
      case _ => false
    }
    def optionType(tp: Type) =
      typeRef(OptionClass.typeConstructor.prefix, OptionClass, List(tp))

    def isSomeType(tp: Type) = tp match {
      case TypeRef(_, sym, List(_)) if sym == SomeClass => true
      case _ => false
    }
    def someType(tp: Type) =
      typeRef(SomeClass.typeConstructor.prefix, SomeClass, List(tp))

    def isNoneType(tp: Type) = tp match {
      case TypeRef(_, sym, List(_)) if sym == NoneClass => true
      case _ => false
    }

    def optionOfProductElems(tp: Type): List[Type] = {
      assert(tp.symbol == OptionClass)
      val prod = tp.typeArgs.head
      if (prod.symbol == UnitClass) List()
      else prod.baseClasses.find { x => isProductType(x.tpe) } match {
        case Some(p) => prod.baseType(p).typeArgs
      }
    }

    def unapplySeqResultToMethodSig(tp: Type) = {
      val ts = optionOfProductElems(tp)
      val last1 = ts.last.baseType(SeqClass) match {
        case TypeRef(pre, seqClass, args) => typeRef(pre, RepeatedParamClass, args)
      }
      ts.init ::: List(last1)
    }

    /** returns unapply or unapplySeq if available */
    def unapplyMember(tp: Type): Symbol = {
      var unapp = tp.member(nme.unapply)
      if (unapp == NoSymbol) unapp = tp.member(nme.unapplySeq)
      unapp
    }
    /* </unapply> */
    val MaxFunctionArity = 9
    val FunctionClass: Array[Symbol] = new Array(MaxFunctionArity + 1)
      def functionApply(n: Int) = getMember(FunctionClass(n), nme.apply)
      def functionType(formals: List[Type], restpe: Type) =
        if (formals.length <= MaxFunctionArity) {
          val sym = FunctionClass(formals.length)
          typeRef(sym.typeConstructor.prefix, sym, formals ::: List(restpe))
        } else NoType;
      def isFunctionType(tp: Type): boolean = tp match {
        case TypeRef(_, sym, args) =>
          (args.length > 0) && (args.length - 1 <= MaxFunctionArity) &&
          (sym == FunctionClass(args.length - 1))
        case _ =>
          false
      }
/*    val RemoteFunctionClass: Array[Symbol] = new Array(MaxFunctionArity + 1)
      def remoteFunctionApply(n: Int) = getMember(RemoteFunctionClass(n), nme.apply)
      def remoteFunctionType(formals: List[Type], restpe: Type) =
        if (formals.length <= MaxFunctionArity) {
          val sym = RemoteFunctionClass(formals.length)
          typeRef(sym.typeConstructor.prefix, sym, formals ::: List(restpe))
        } else NoType;
      def isRemoteFunctionType(tp: Type): boolean = tp match {
        case TypeRef(_, sym, args) =>
          (args.length > 0) && (args.length - 1 <= MaxFunctionArity) &&
          (sym == RemoteFunctionClass(args.length - 1))
        case _ =>
          false
      }
*/
    def seqType(arg: Type) =
      typeRef(SeqClass.typeConstructor.prefix, SeqClass, List(arg))

    def NilModule: Symbol = getModule("scala.Nil")
    def ConsClass: Symbol = getClass("scala.$colon$colon")

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
      def Object_equals   = getMember(ObjectClass, nme.equals_)
      def Object_hashCode = getMember(ObjectClass, nme.hashCode_)
      def Object_toString = getMember(ObjectClass, nme.toString_)

    var String_+           : Symbol = _

    // members of class scala.Iterator
    var Iterator_next      : Symbol = _
    var Iterator_hasNext   : Symbol = _

    // pattern wildcard
    var PatternWildcard: Symbol = _

    // boxed classes
    var BoxedArrayClass: Symbol = _
    var BoxedAnyArrayClass: Symbol = _
    var BoxedObjectArrayClass: Symbol = _
    var BoxedUnitClass: Symbol = _
    var BoxedUnitModule: Symbol = _
      def BoxedUnit_UNIT = getMember(BoxedUnitModule, "UNIT")
    var ObjectRefClass: Symbol = _

    // special attributes
    var SerializableAttr: Symbol = _
    var BeanPropertyAttr: Symbol = _
    var AnnotationDefaultAttr: Symbol = _

    def getModule(fullname: Name): Symbol = getModuleOrClass(fullname, true)

    def getClass(fullname: Name): Symbol = getModuleOrClass(fullname, false)

    def getMember(owner: Symbol, name: Name) = {
      val result = owner.info.nonPrivateMember(name)
      if (result == NoSymbol) {
        Console.println(owner.infosString)
        Console.println(owner.info.decls)
        throw new FatalError(owner.toString() + " does not have a member " + name)
      }
      result
    }

    private def getModuleOrClass(fullname: Name, module: boolean): Symbol = {
      var sym = RootClass
      var i = 0
      var j = fullname.pos('.', i)
      while (j < fullname.length) {
        sym = sym.info.member(fullname.subName(i, j))
        i = j + 1
        j = fullname.pos('.', i)
      }
      val result =
        if (module) sym.info.member(fullname.subName(i, j)).suchThat(.hasFlag(MODULE))
        else sym.info.member(fullname.subName(i, j).toTypeName)
      if (result == NoSymbol) {
        if (settings.debug.value)
          { Console.println(sym.info); Console.println(sym.info.members) }//debug
        throw new FatalError((if (module) "object " else "class ") + fullname + " not found.")
      }
      result
    }

    private def newClass(owner: Symbol, name: Name, parents: List[Type]): Symbol = {
      val clazz = owner.newClass(NoPos, name.toTypeName)
      clazz.setInfo(ClassInfoType(parents, newScope, clazz))
      owner.info.decls.enter(clazz)
      clazz
    }

    private def newCovariantPolyClass(owner: Symbol, name: Name, parent: Symbol => Type): Symbol = {
      val clazz = newClass(owner, name, List())
      val tparam = newTypeParam(clazz, 0) setFlag COVARIANT
      clazz.setInfo(
        PolyType(
          List(tparam),
          ClassInfoType(List(parent(tparam)), newScope, clazz)))
    }

    private def newAlias(owner: Symbol, name: Name, alias: Type): Symbol = {
      val tpsym = owner.newAliasType(NoPos, name.toTypeName)
      tpsym.setInfo(alias)
      owner.info.decls.enter(tpsym)
      tpsym
    }

    private def newMethod(owner: Symbol, name: Name): Symbol = {
      val msym = owner.newMethod(NoPos, name.encode)
      owner.info.decls.enter(msym)
      msym
    }

    private def newMethod(owner: Symbol, name: Name, formals: List[Type], restpe: Type): Symbol =
      newMethod(owner, name).setInfo(MethodType(formals, restpe))

    private def newPolyMethod(owner: Symbol, name: Name, tcon: Symbol => Type): Symbol = {
      val msym = newMethod(owner, name)
      val tparam = newTypeParam(msym, 0)
      msym.setInfo(PolyType(List(tparam), tcon(tparam)))
    }

    private def newParameterlessMethod(owner: Symbol, name: Name, restpe: Type) =
      newMethod(owner, name).setInfo(PolyType(List(),restpe))

    private def newTypeParam(owner: Symbol, index: int): Symbol =
      owner.newTypeParameter(NoPos, "T" + index)
        .setInfo(TypeBounds(AllClass.typeConstructor, AnyClass.typeConstructor))

    val boxedClass = new HashMap[Symbol, Symbol]
    val unboxMethod = new HashMap[Symbol, Symbol] // Type -> Method
    val isUnbox = new HashSet[Symbol]
    val boxMethod = new HashMap[Symbol, Symbol] // Type -> Method
    val isBox = new HashSet[Symbol]
    val boxedArrayClass = new HashMap[Symbol, Symbol]

    val refClass = new HashMap[Symbol, Symbol]
    private val abbrvTag = new HashMap[Symbol, char]

    private def newValueClass(name: Name, tag: char): Symbol = {
      def boxedName: String =
        "scala.runtime.Boxed" + name
      val clazz =
        newClass(ScalaPackageClass, name, List(AnyValClass.typeConstructor))
        .setFlag(ABSTRACT | SEALED)
      boxedClass(clazz) = getClass(boxedName)
      boxedArrayClass(clazz) = getClass("scala.runtime.Boxed" + name + "Array")
      refClass(clazz) = getClass("scala.runtime." + name + "Ref")
      abbrvTag(clazz) = tag

      val module = ScalaPackageClass.newModule(NoPos, name)
      ScalaPackageClass.info.decls.enter(module)
      val mclass = module.moduleClass
      mclass.setInfo(ClassInfoType(List(), newScope, mclass))
      module.setInfo(mclass.tpe)
      val box = newMethod(mclass, nme.box, List(clazz.typeConstructor),
                          ObjectClass.typeConstructor)
      boxMethod(clazz) = box
      isBox += box
      val unbox = newMethod(mclass, nme.unbox, List(ObjectClass.typeConstructor),
                            clazz.typeConstructor)
      unboxMethod(clazz) = unbox
      isUnbox += unbox

      clazz
    }

    private def initValueClasses: Unit = {
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

      val floattype = if (forCLDC) null else FloatClass.typeConstructor
      val floatparam = if (forCLDC) null else List(floattype)
      val doubletype = if (forCLDC) null else DoubleClass.typeConstructor
      val doubleparam = if (forCLDC) null else List(doubletype)

      val stringtype = StringClass.typeConstructor

      // init scala.Boolean
      newParameterlessMethod(BooleanClass, nme.ZNOT, booltype)
      newMethod(BooleanClass, nme.EQ,   boolparam, booltype)
      newMethod(BooleanClass, nme.NE,   boolparam, booltype)
      newMethod(BooleanClass, nme.ZOR,  boolparam, booltype)
      newMethod(BooleanClass, nme.ZAND, boolparam, booltype)
      newMethod(BooleanClass, nme.OR,   boolparam, booltype)
      newMethod(BooleanClass, nme.AND,  boolparam, booltype)
      newMethod(BooleanClass, nme.XOR,  boolparam, booltype)

      def initValueClass(clazz: Symbol, isCardinal: Boolean): Unit = {
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

        if (!forCLDC) {
          newParameterlessMethod(clazz, nme.toFloat,  floattype)
          newParameterlessMethod(clazz, nme.toDouble, doubletype)
        }

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
        newParameterlessMethod(clazz, nme.ADD, restype)
        newParameterlessMethod(clazz, nme.SUB, restype)
        if (isCardinal)
          newParameterlessMethod(clazz, nme.NOT, restype)

        // binary operations
        val restype2 = if (isCardinal) longtype else restype
        addBinops(byteparam,   restype,    isCardinal)
        addBinops(shortparam,  restype,    isCardinal)
        addBinops(charparam,   restype,    isCardinal)
        addBinops(intparam,    restype,    isCardinal)
        addBinops(longparam,   restype2,   isCardinal)
        if (!forCLDC) {
          val restype3 = if (clazz eq DoubleClass) doubletype else floattype
          addBinops(floatparam,  restype3,   false)
          addBinops(doubleparam, doubletype, false)
        }
      }

      initValueClass(ByteClass,   true)
      initValueClass(ShortClass,  true)
      initValueClass(CharClass,   true)
      initValueClass(IntClass,    true)
      initValueClass(LongClass,   true)
      if (!forCLDC) {
        initValueClass(FloatClass,  false)
        initValueClass(DoubleClass, false)
      }
    }

    /** Is symbol a value class? */
    def isValueClass(sym: Symbol): boolean =
      (sym eq UnitClass) || (boxedClass contains sym)

    /** Is symbol a value class? */
    def isNumericValueClass(sym: Symbol): boolean =
      (sym ne BooleanClass) && (boxedClass contains sym)

    /** Is symbol a value or array class? */
    def isUnboxedClass(sym: Symbol): boolean =
      isValueClass(sym) || sym == ArrayClass

    def signature(tp: Type): String = {
      def erasure(tp: Type): Type = tp match {
        case st: SubType => erasure(st.supertype)
        case RefinedType(parents, _) => erasure(parents.head)
        case _ => tp
      }
      def flatNameString(sym: Symbol, separator: char): String =
        if (sym.owner.isPackageClass) sym.fullNameString('.')
        else flatNameString(sym.owner, separator) + "$" + sym.simpleName;
      def signature1(etp: Type): String = {
        if (etp.symbol == ArrayClass) "[" + signature1(erasure(etp.typeArgs.head))
        else if (isValueClass(etp.symbol)) abbrvTag(etp.symbol).toString()
        else "L" + flatNameString(etp.symbol, '/') + ";"
      }
      val etp = erasure(tp)
      if (etp.symbol == ArrayClass) signature1(etp)
      else flatNameString(etp.symbol, '.')
    }

    private var isInitialized = false

    def init: unit = {
      if (isInitialized) return
      isInitialized = true
      RootClass =
        NoSymbol.newClass(NoPos, nme.ROOT.toTypeName)
          .setFlag(FINAL | MODULE | PACKAGE | JAVA).setInfo(rootLoader)
      RootPackage = NoSymbol.newValue(NoPos, nme.ROOTPKG)
          .setFlag(FINAL | MODULE | PACKAGE | JAVA)
          .setInfo(PolyType(List(), RootClass.tpe))

      EmptyPackage =
        RootClass.newPackage(NoPos, nme.EMPTY_PACKAGE_NAME).setFlag(FINAL)
      EmptyPackageClass = EmptyPackage.moduleClass
      EmptyPackageClass.setInfo(ClassInfoType(List(), newScope, EmptyPackageClass))

      EmptyPackage.setInfo(EmptyPackageClass.tpe)
      RootClass.info.decls.enter(EmptyPackage)
      RootClass.info.decls.enter(RootPackage)

      JavaLangPackage = getModule("java.lang")
      ScalaPackage = getModule("scala")
      assert(ScalaPackage ne null, "Scala package is null")
      ScalaPackageClass = ScalaPackage.tpe.symbol

      AnyClass = newClass(ScalaPackageClass, nme.Any, List()).setFlag(ABSTRACT)

      val anyparam = List(AnyClass.typeConstructor)

      AnyValClass = newClass(ScalaPackageClass, nme.AnyVal, anyparam)
        .setFlag(FINAL | SEALED)

      ObjectClass = getClass("java.lang.Object")

      AnyRefClass =
        newAlias(ScalaPackageClass, nme.AnyRef, ObjectClass.typeConstructor)

      val anyrefparam = List(AnyRefClass.typeConstructor)

      AllRefClass = newClass(ScalaPackageClass, nme.Null, anyrefparam)
        .setFlag(ABSTRACT | TRAIT | FINAL)

      AllClass = newClass(ScalaPackageClass, nme.Nothing, anyparam)
        .setFlag(ABSTRACT | TRAIT | FINAL)

      StringClass = getClass("java.lang.String")

      ClassClass = getClass("java.lang.Class")
      ThrowableClass = getClass("java.lang.Throwable")
      NullPointerExceptionClass = getClass("java.lang.NullPointerException")
      NonLocalReturnExceptionClass = getClass("scala.runtime.NonLocalReturnException")

      UnitClass =
        newClass(ScalaPackageClass, nme.Unit, List(AnyValClass.typeConstructor))
      abbrvTag(UnitClass) = 'V'

      BooleanClass = newValueClass(nme.Boolean, 'Z')
      ByteClass =    newValueClass(nme.Byte, 'B')
      ShortClass =   newValueClass(nme.Short, 'S')
      CharClass =    newValueClass(nme.Char, 'C')
      IntClass =     newValueClass(nme.Int, 'I')
      LongClass =    newValueClass(nme.Long, 'L')
      if (!forCLDC) {
        FloatClass =   newValueClass(nme.Float, 'F')
        DoubleClass =  newValueClass(nme.Double, 'D')
      }

      // the scala reference classes
      ScalaObjectClass = getClass("scala.ScalaObject")
      AttributeClass = getClass("scala.Attribute")
      //ChannelClass = getClass("scala.distributed.Channel")
      //RemoteRefClass = getClass("scala.distributed.RemoteRef")
      if (!forCLDC) {
        CodeClass = getClass("scala.reflect.Code")
        CodeModule = getModule("scala.reflect.Code")
      }
      PartialFunctionClass = getClass("scala.PartialFunction")
      ByNameFunctionClass = getClass("scala.ByNameFunction")
      IterableClass = getClass("scala.Iterable")
      IteratorClass = getClass("scala.Iterator")
      SeqClass = getClass("scala.Seq")
      ListClass = getClass("scala.List")
      ListModule = getModule("scala.List")
      ArrayClass = getClass("scala.Array")
      SerializableClass = if (forCLDC) null else getClass("java.io.Serializable")
      PredefModule = getModule("scala.Predef")
      ConsoleModule = getModule("scala.Console")
      MatchErrorClass = getClass("scala.MatchError")
      MatchErrorModule = getModule("scala.MatchError")
      IndexOutOfBoundsExceptionClass = getClass("java.lang.IndexOutOfBoundsException")
      //RemoteExecutionModule = getModule("scala.distributed.RemoteExecution")
      ScalaRunTimeModule = getModule("scala.runtime.ScalaRunTime")
      RepeatedParamClass = newCovariantPolyClass(
        ScalaPackageClass, nme.REPEATED_PARAM_CLASS_NAME,
        tparam => typeRef(SeqClass.typeConstructor.prefix, SeqClass, List(tparam.typeConstructor)))
      ByNameParamClass = newCovariantPolyClass(
        ScalaPackageClass, nme.BYNAME_PARAM_CLASS_NAME, tparam => AnyClass.typeConstructor)
      /* <unapply> */
      OptionClass = getClass("scala.Option")

      for (val i <- 1 to MaxTupleArity) {
        TupleClass(i)   = getClass(  "scala.Tuple" + i)
      }
      for (val i <- 1 to MaxProductArity) {
        ProductClass(i) = getClass("scala.Product" + i)
      }
      /* </unapply> */
      for (val i <- 0 to MaxFunctionArity) {
        FunctionClass(i) = getClass("scala.Function" + i)
        //RemoteFunctionClass(i) = getClass("scala.distributed.RemoteFunction" + i)
      }
      initValueClasses

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
        AnyClass, nme.isInstanceOf, tparam => booltype) setFlag FINAL
      Any_asInstanceOf = newPolyMethod(
        AnyClass, nme.asInstanceOf, tparam => tparam.typeConstructor) setFlag FINAL
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
      Object_synchronized = newPolyMethod(
        ObjectClass, nme.synchronized_,
        tparam => MethodType(List(tparam.typeConstructor), tparam.typeConstructor)) setFlag FINAL
      Object_isInstanceOf = newPolyMethod(
        ObjectClass, "$isInstanceOf",
        tparam => MethodType(List(), booltype)) setFlag FINAL
      Object_asInstanceOf = newPolyMethod(
        ObjectClass, "$asInstanceOf",
        tparam => MethodType(List(), tparam.typeConstructor)) setFlag FINAL
      String_+ = newMethod(
        StringClass, "+", anyparam, StringClass.typeConstructor) setFlag FINAL

      PatternWildcard = NoSymbol.newValue(NoPos, "_").setInfo(AllClass.typeConstructor)

      BoxedArrayClass = getClass("scala.runtime.BoxedArray")
      BoxedAnyArrayClass = getClass("scala.runtime.BoxedAnyArray")
      BoxedObjectArrayClass = getClass("scala.runtime.BoxedObjectArray")
      BoxedUnitClass = getClass("scala.runtime.BoxedUnit")
      BoxedUnitModule = getModule("scala.runtime.BoxedUnit")
      ObjectRefClass = getClass("scala.runtime.ObjectRef")

      AnnotationDefaultAttr = newClass(RootClass,
                                       nme.AnnotationDefaultATTR,
                                       List(AttributeClass.typeConstructor))
      SerializableAttr = getClass("scala.serializable")
      BeanPropertyAttr = if (forCLDC) null else getClass("scala.reflect.BeanProperty")
    }
  }
}
