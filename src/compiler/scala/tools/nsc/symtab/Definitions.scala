/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.symtab;

import scala.tools.nsc.util.Position;
import collection.mutable.HashMap;
import Flags._;

mixin class Definitions requires SymbolTable {

  object definitions {
    def isDefinitionsInitialized = isInitialized;


    // root packages and classes
    var RootClass: Symbol = _;
    var EmptyPackage: Symbol = _;
    var EmptyPackageClass: Symbol = _;
    var emptypackagescope: Scope = null; //debug

    var JavaPackage: Symbol = _;
    var JavaLangPackage: Symbol = _;
    var ScalaPackage: Symbol = _;
    var ScalaPackageClass: Symbol = _;

    var AnyClass: Symbol = _;
    var AnyValClass: Symbol = _;
    var ObjectClass: Symbol = _;

    var AnyRefClass: Symbol = _;

    var AllRefClass: Symbol = _;
    var AllClass: Symbol = _;

    var StringClass: Symbol = _;
    var ThrowableClass: Symbol = _;
    var NullPointerExceptionClass: Symbol = _;

    // the scala value classes
    var UnitClass: Symbol = _;
    var BooleanClass: Symbol = _;
      def Boolean_not = getMember(BooleanClass, nme.ZNOT);
      def Boolean_and = getMember(BooleanClass, nme.ZAND);
      def Boolean_or  = getMember(BooleanClass, nme.ZOR);
    var ByteClass: Symbol = _;
    var ShortClass: Symbol = _;
    var CharClass: Symbol = _;
    var IntClass: Symbol = _;
    var LongClass: Symbol = _;
    var FloatClass: Symbol = _;
    var DoubleClass: Symbol = _;

    // the scala reference classes
    var ScalaObjectClass: Symbol = _;
      def ScalaObjectClass_tag = getMember(ScalaObjectClass,  nme.tag );
    var AttributeClass: Symbol = _;
    var RefClass: Symbol = _;
    var TypedCodeClass: Symbol = _;
    var PartialFunctionClass: Symbol = _;
    var IterableClass: Symbol = _;
      def Iterable_next = getMember(IterableClass, "next");
      def Iterable_hasNext = getMember(IterableClass, "hasNext");
    var IteratorClass: Symbol = _;
    var SeqClass: Symbol = _;
      def Seq_length = getMember(SeqClass, "length");
    var ListClass: Symbol = _;
      def List_isEmpty = getMember(ListClass, "isEmpty");
      def List_head = getMember(ListClass, "head");
      def List_tail = getMember(ListClass, "tail");
    var ArrayClass: Symbol = _;
    var TypeClass: Symbol = _;
    var SerializableClass: Symbol = _;
    //var NonNullClass: Symbol = _;
    var PredefModule: Symbol = _;
    var ConsoleModule: Symbol = _;
    var MatchErrorClass: Symbol = _;
    var MatchErrorModule: Symbol = _;
      def MatchError_fail = getMember(MatchErrorModule, "fail");
      def MatchError_report = getMember(MatchErrorModule, "report");
    var ScalaRunTimeModule: Symbol = _;
      def SeqFactory = getMember(ScalaRunTimeModule, "Seq");
      def checkDefinedMethod = getMember(ScalaRunTimeModule, "checkDefined");
    var RepeatedParamClass: Symbol = _;
    var ByNameParamClass: Symbol = _;
    //var TraitClass: Symbol = _;

    val MaxTupleArity = 9;
    val MaxFunctionArity = 9;
    var TupleClass: Array[Symbol] = _;
      def tupleField(n:int, j:int) = getMember(TupleClass(n), "_" + j);
    var FunctionClass: Array[Symbol] = _;
      def functionApply(n:int) = getMember(FunctionClass(n), "apply");

    def tupleType(elems: List[Type]) =
      if (elems.length <= MaxTupleArity) {
	val sym = TupleClass(elems.length);
	typeRef(sym.typeConstructor.prefix, sym, elems)
      } else NoType;


    def functionType(formals: List[Type], restpe: Type) =
      if (formals.length <= MaxFunctionArity) {
	val sym = FunctionClass(formals.length);
	typeRef(sym.typeConstructor.prefix, sym, formals ::: List(restpe))
      } else NoType;


    def isTupleType(tp: Type): boolean = tp match {
      case TypeRef(_, sym, elems) =>
        elems.length <= MaxTupleArity && sym == TupleClass(elems.length);
      case _ =>
        false
    }

    def isFunctionType(tp: Type): boolean = tp match {
      case TypeRef(_, sym, args) =>
        ((args.length > 0) && (args.length - 1 <= MaxFunctionArity) &&
        (sym == FunctionClass(args.length - 1)))
      case _ =>
        false
    }

    def seqType(arg: Type) =
      typeRef(SeqClass.typeConstructor.prefix, SeqClass, List(arg));

    def NilModule: Symbol = getModule("scala.Nil");
    def ConsClass: Symbol = getClass("scala.$colon$colon");

    // members of class scala.Any
    var Any_==          : Symbol = _;
    var Any_!=          : Symbol = _;
    var Any_equals      : Symbol = _;
    var Any_hashCode    : Symbol = _;
    var Any_toString    : Symbol = _;
    var Any_isInstanceOf: Symbol = _;
    var Any_asInstanceOf: Symbol = _;
    var Any_isInstanceOfErased: Symbol = _;
    var Any_asInstanceOfErased: Symbol = _;

    // members of class java.lang.{Object, String}
    var Object_eq          : Symbol = _;
    var Object_ne          : Symbol = _;
    var Object_==          : Symbol = _;
    var Object_!=          : Symbol = _;
    var Object_synchronized: Symbol = _;
    var Object_isInstanceOf: Symbol = _;
    var Object_asInstanceOf: Symbol = _;
      def Object_equals   = getMember(ObjectClass, nme.equals_);
      def Object_hashCode = getMember(ObjectClass, nme.hashCode_);
      def Object_toString = getMember(ObjectClass, nme.toString_);

    var String_+           : Symbol = _;

    // members of class scala.Iterator
    var Iterator_next      : Symbol = _;
    var Iterator_hasNext   : Symbol = _;

    // pattern wildcard
    var PatternWildcard: Symbol = _;

    // boxed classes
    var BoxedArrayClass: Symbol = _;
    var BoxedAnyArrayClass: Symbol = _;
    var BoxedObjectArrayClass: Symbol = _;
    var BoxedNumberClass: Symbol = _;
    var BoxedUnitClass: Symbol = _;
    var BoxedUnitModule: Symbol = _;
      def BoxedUnit_UNIT = getMember(BoxedUnitModule, "UNIT");
    var ObjectRefClass: Symbol = _;

    // special attributes
    var SerializableAttr: Symbol = _;
    var BeanPropertyAttr: Symbol = _;

    def getModule(fullname: Name): Symbol =
      getModuleOrClass(fullname, true);

    def getClass(fullname: Name): Symbol =
      getModuleOrClass(fullname, false);

    def getMember(owner: Symbol, name: Name) = {
      val result = owner.info.nonPrivateMember(name);
      if (result == NoSymbol)
    	throw new FatalError(owner.toString() + " does not have a member " + name);
      result
    }

    private def getModuleOrClass(fullname: Name, module: boolean): Symbol = {
      var sym = RootClass;
      var i = 0;
      var j = fullname.pos('.', i);
      while (j < fullname.length) {
        sym = sym.info.member(fullname.subName(i, j));
        i = j + 1;
        j = fullname.pos('.', i)
      }
      val result =
        if (module) sym.info.member(fullname.subName(i, j)).suchThat(.hasFlag(MODULE));
        else sym.info.member(fullname.subName(i, j).toTypeName);
      if (result == NoSymbol) {
        System.out.println(sym.info);
        System.out.println(sym.info.members);
	throw new FatalError((if (module) "object " else "class ") + fullname + " not found.");
      }
      result
    }

    private def newClass(owner: Symbol, name: Name, parents: List[Type]): Symbol = {
      val clazz = owner.newClass(Position.NOPOS, name.toTypeName);
      clazz.setInfo(ClassInfoType(parents, new Scope(), clazz));
      owner.info.decls.enter(clazz);
      clazz
    }

    private def newCovariantPolyClass(owner: Symbol, name: Name, parent: Symbol => Type): Symbol = {
      val clazz = newClass(owner, name, List());
      val tparam = newTypeParam(clazz, 0) setFlag COVARIANT;
      clazz.setInfo(
	PolyType(
	  List(tparam),
	  ClassInfoType(List(parent(tparam)), new Scope(), clazz)))
    }

    private def newAlias(owner: Symbol, name: Name, alias: Type): Symbol = {
      val tpsym = owner.newAliasType(Position.NOPOS, name.toTypeName);
      tpsym.setInfo(alias);
      owner.info.decls.enter(tpsym);
      tpsym
    }

    private def newMethod(owner: Symbol, name: Name): Symbol = {
      val msym = owner.newMethod(Position.NOPOS, name.encode);
      owner.info.decls.enter(msym);
      msym
    }

    private def newMethod(owner: Symbol, name: Name, formals: List[Type], restpe: Type): Symbol =
      newMethod(owner, name).setInfo(MethodType(formals, restpe));

    private def newPolyMethod(owner: Symbol, name: Name, tcon: Symbol => Type): Symbol = {
      val msym = newMethod(owner, name);
      val tparam = newTypeParam(msym, 0);
      msym.setInfo(PolyType(List(tparam), tcon(tparam)))
    }

    private def newTypeParam(owner: Symbol, index: int): Symbol =
      owner.newTypeParameter(Position.NOPOS, "T" + index)
        .setInfo(TypeBounds(AllClass.typeConstructor, AnyClass.typeConstructor));

    val boxedClass = new HashMap[Symbol, Symbol];
    val boxedArrayClass = new HashMap[Symbol, Symbol];
    val refClass = new HashMap[Symbol, Symbol];
    private val abbrvTag = new HashMap[Symbol, char];

    private def getValueClass(name: String, tag: char): Symbol = {
      val result = getClass("scala." + name);
      boxedClass(result) = getClass("scala.runtime.Boxed" + name);
      if (name != "Unit") {
        boxedArrayClass(result) = getClass("scala.runtime.Boxed" + name + "Array");
        refClass(result) = getClass("scala.runtime." + name + "Ref");
      }
      abbrvTag(result) = tag;
      result
    }

    /** Is symbol a value class? */
    def isValueClass(sym: Symbol): boolean = boxedClass contains sym;

    /** Is symbol a value class? */
    def isNumericValueClass(sym: Symbol): boolean =
      isValueClass(sym) && sym != BooleanClass && sym != UnitClass;

    /** Is symbol a value or array class? */
    def isUnboxedClass(sym: Symbol): boolean = isValueClass(sym) || sym == ArrayClass;

    def signature(tp: Type): String = {
      def flatNameString(sym: Symbol, separator: char): String =
        if (sym.owner.isPackageClass) sym.fullNameString('.')
        else flatNameString(sym.owner, separator) + "$" + sym.simpleName;
      def signature1(tp: Type): String = {
        if (tp.symbol == ArrayClass) "[" + signature1(tp.typeArgs.head);
        else if (isValueClass(tp.symbol)) String.valueOf(abbrvTag(tp.symbol))
        else "L" + flatNameString(tp.symbol, '/') + ";"
      }
      if (tp.symbol == ArrayClass) signature1(tp);
      else flatNameString(tp.symbol, '.')
    }

    private var isInitialized = false;

    def init: unit = {
      if (isInitialized) return;
      isInitialized = true;
      RootClass =
	NoSymbol.newClass(Position.NOPOS, nme.ROOT.toTypeName)
	  .setFlag(FINAL | MODULE | PACKAGE | JAVA).setInfo(rootLoader);

      EmptyPackage =
        RootClass.newPackage(Position.NOPOS, nme.EMPTY_PACKAGE_NAME).setFlag(FINAL);
      EmptyPackageClass = EmptyPackage.moduleClass;
      EmptyPackageClass.setInfo(ClassInfoType(List(), new Scope(), EmptyPackageClass));

      EmptyPackage.setInfo(EmptyPackageClass.tpe);
      RootClass.info.decls.enter(EmptyPackage);

      JavaPackage = getModule("java");
      JavaLangPackage = getModule("java.lang");
      ScalaPackage = getModule("scala");
      assert(ScalaPackage != null, "Scala package is null");
      ScalaPackageClass = ScalaPackage.tpe.symbol;

      AnyClass = newClass(ScalaPackageClass, "Any", List());
      AnyValClass = getClass("scala.AnyVal") setFlag SEALED;
      ObjectClass = getClass("java.lang.Object");

      AnyRefClass = newAlias(ScalaPackageClass, "AnyRef", ObjectClass.typeConstructor);

      AllRefClass = newClass(ScalaPackageClass, "AllRef", List(AnyRefClass.typeConstructor))
	.setFlag(ABSTRACT | MIXIN | FINAL);

      AllClass = newClass(ScalaPackageClass, "All", List(AnyClass.typeConstructor))
	.setFlag(ABSTRACT | MIXIN | FINAL);

      StringClass = getClass("java.lang.String");
      ThrowableClass = getClass("java.lang.Throwable");
      NullPointerExceptionClass = getClass("java.lang.NullPointerException");

      // the scala value classes
      UnitClass = getValueClass("Unit", 'V');
      BooleanClass = getValueClass("Boolean", 'Z');
      ByteClass = getValueClass("Byte", 'B');
      ShortClass = getValueClass("Short", 'S');
      CharClass = getValueClass("Char", 'C');
      IntClass = getValueClass("Int", 'I');
      LongClass = getValueClass("Long", 'L');
      FloatClass = getValueClass("Float", 'F');
      DoubleClass = getValueClass("Double", 'D');

      // the scala reference classes
      ScalaObjectClass = getClass("scala.ScalaObject");
      AttributeClass = getClass("scala.Attribute");
      RefClass = getClass("scala.Ref");
      TypedCodeClass = getClass("scala.reflect.TypedCode");
      PartialFunctionClass = getClass("scala.PartialFunction");
      IterableClass = getClass("scala.Iterable");
      IteratorClass = getClass("scala.Iterator");
      SeqClass = getClass("scala.Seq");
      ListClass = getClass("scala.List");
      ArrayClass = getClass("scala.Array");
      //TypeClass = getClass("scala.Type");
      SerializableClass = getClass("java.io.Serializable");
      //NonNullClass = getClass("scala.NonNull");
      PredefModule = getModule("scala.Predef");
      ConsoleModule = getModule("scala.Console");
      MatchErrorClass = getClass("scala.MatchError");
      MatchErrorModule = getModule("scala.MatchError");
      ScalaRunTimeModule = getModule("scala.runtime.ScalaRunTime");
      RepeatedParamClass = newCovariantPolyClass(
        ScalaPackageClass, nme.REPEATED_PARAM_CLASS_NAME,
        tparam => typeRef(SeqClass.typeConstructor.prefix, SeqClass, List(tparam.typeConstructor)));
      ByNameParamClass = newCovariantPolyClass(
        ScalaPackageClass, nme.BYNAME_PARAM_CLASS_NAME, tparam => AnyClass.typeConstructor);
      //TraitClass = getClass("scala._trait_");
      TupleClass = new Array(MaxTupleArity + 1);
      for (val i <- List.range(1, MaxTupleArity + 1))
	TupleClass(i) = getClass("scala.Tuple" + i);
      FunctionClass = new Array(MaxFunctionArity + 1);
      for (val i <- List.range(0, MaxFunctionArity + 1))
	FunctionClass(i) = getClass("scala.Function" + i);

      // members of class scala.Any
      Any_== = newMethod(
        AnyClass, "==", List(AnyClass.typeConstructor), BooleanClass.typeConstructor) setFlag FINAL;
      Any_!= = newMethod(
        AnyClass, "!=", List(AnyClass.typeConstructor), BooleanClass.typeConstructor) setFlag FINAL;
      Any_equals = newMethod(
        AnyClass, "equals", List(AnyClass.typeConstructor), BooleanClass.typeConstructor);
      Any_hashCode = newMethod(
        AnyClass, "hashCode", List(), IntClass.typeConstructor);
      Any_toString = newMethod(
        AnyClass, "toString", List(), StringClass.typeConstructor);

      Any_isInstanceOf = newPolyMethod(
        AnyClass, "isInstanceOf", tparam => BooleanClass.typeConstructor) setFlag FINAL;
      Any_asInstanceOf = newPolyMethod(
        AnyClass, "asInstanceOf", tparam => tparam.typeConstructor) setFlag FINAL;
      Any_isInstanceOfErased = newPolyMethod(
        AnyClass, "isInstanceOf$erased", tparam => BooleanClass.typeConstructor) setFlag FINAL;
      Any_asInstanceOfErased = newPolyMethod(
        AnyClass, "asInstanceOf$erased", tparam => tparam.typeConstructor) setFlag FINAL;

      // members of class java.lang.{Object, String}
      Object_== = newMethod(
        ObjectClass, "==", List(AnyRefClass.typeConstructor), BooleanClass.typeConstructor) setFlag FINAL;
      Object_!= = newMethod(
        ObjectClass, "!=", List(AnyRefClass.typeConstructor), BooleanClass.typeConstructor) setFlag FINAL;
      Object_eq = newMethod(
        ObjectClass, "eq", List(AnyRefClass.typeConstructor), BooleanClass.typeConstructor) setFlag FINAL;
      Object_ne = newMethod(
        ObjectClass, "ne", List(AnyRefClass.typeConstructor), BooleanClass.typeConstructor) setFlag FINAL;
      Object_synchronized = newPolyMethod(
        ObjectClass, "synchronized", tparam => MethodType(List(tparam.typeConstructor), tparam.typeConstructor)) setFlag FINAL;
      Object_isInstanceOf = newPolyMethod(
	ObjectClass, "$isInstanceOf",
        tparam => MethodType(List(), BooleanClass.typeConstructor)) setFlag FINAL;
      Object_asInstanceOf = newPolyMethod(
	ObjectClass, "$asInstanceOf",
        tparam => MethodType(List(), tparam.typeConstructor)) setFlag FINAL;
      String_+ = newMethod(
        StringClass, "+", List(AnyClass.typeConstructor), StringClass.typeConstructor) setFlag FINAL;

      PatternWildcard = NoSymbol.newValue(Position.NOPOS, "_").setInfo(AllClass.typeConstructor);

      BoxedArrayClass = getClass("scala.runtime.BoxedArray");
      BoxedAnyArrayClass = getClass("scala.runtime.BoxedAnyArray");
      BoxedObjectArrayClass = getClass("scala.runtime.BoxedObjectArray");
      BoxedNumberClass = getClass("scala.runtime.BoxedNumber");
      BoxedUnitClass = getClass("scala.runtime.BoxedUnit");
      BoxedUnitModule = getModule("scala.runtime.BoxedUnit");
      ObjectRefClass = getClass("scala.runtime.ObjectRef");

      SerializableAttr = getClass("scala.serializable");
      BeanPropertyAttr = getClass("scala.reflect.BeanProperty");
    }
  }
}
