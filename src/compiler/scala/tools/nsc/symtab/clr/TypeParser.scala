/* NSC -- new scala compiler
 * Copyright 2004-2010 LAMP/EPFL
 */


package scala.tools.nsc
package symtab
package clr

import java.io.IOException

import ch.epfl.lamp.compiler.msil.{Type => MSILType, Attribute => MSILAttribute, _}

import scala.collection.mutable.{HashMap, HashSet}
import classfile.UnPickler

/**
 *  @author Nikolay Mihaylov
 */
abstract class TypeParser {

  val global: Global

  import global._
  import loaders.clrTypes

  //##########################################################################

  private var clazz: Symbol = _
  private var instanceDefs: Scope = _   // was members
  private var staticModule: Symbol = _  // was staticsClass
  private var staticDefs: Scope = _     // was statics

  protected def statics: Symbol = staticModule.moduleClass

  protected var busy: Boolean = false       // lock to detect recursive reads

  private object unpickler extends UnPickler {
    val global: TypeParser.this.global.type = TypeParser.this.global
  }

  def parse(typ: MSILType, root: Symbol) {

    def handleError(e: Throwable) = {
      if (settings.debug.value) e.printStackTrace()  //debug
      throw new IOException("type '" + typ.FullName + "' is broken\n(" + e.getMessage() + ")")
    }
    assert(!busy)
    busy = true

    if (root.isModule) {
      this.clazz = root.companionClass
      this.staticModule = root
    } else {
      this.clazz = root
      this.staticModule = root.companionModule
    }
    try {
      parseClass(typ)
    } catch {
      case e: FatalError => handleError(e)
      case e: RuntimeException => handleError(e)
    }
    busy = false
  }

  private def parseClass(typ: MSILType) {

    clrTypes.types(clazz) = typ
    clrTypes.sym2type(typ) = clazz

    if (typ.IsDefined(clrTypes.SCALA_SYMTAB_ATTR, false)) {
      val attrs = typ.GetCustomAttributes(clrTypes.SCALA_SYMTAB_ATTR, false);
      assert (attrs.length == 1, attrs.length);
      val a = attrs(0).asInstanceOf[MSILAttribute];
      assert (a.getConstructor() == clrTypes.SYMTAB_CONSTR);
      val symtab = a.getConstructorArguments()(0).asInstanceOf[Array[Byte]]
      unpickler.unpickle(symtab, 0, clazz, staticModule, typ.FullName);
      val mClass = clrTypes.getType(typ.FullName + "$");
      if (mClass != null) {
        clrTypes.types(statics) = mClass;
        val moduleInstance = mClass.GetField("MODULE$");
        assert (moduleInstance != null, mClass);
        clrTypes.fields(statics) = moduleInstance;
      }
      return
    }
    val flags = translateAttributes(typ)
    val ifaces: Array[MSILType] = typ.getInterfaces()
    val superType = if (typ.BaseType() != null) getCLRType(typ.BaseType())
                    else  if (typ.IsInterface()) definitions.ObjectClass.tpe
                    else definitions.AnyClass.tpe; // this is System.Object
    val parents = superType :: ifaces.map(getCLRType).toList
    instanceDefs = new Scope
    staticDefs = new Scope

    val classInfo = ClassInfoType(parents, instanceDefs, clazz)
    val staticInfo = ClassInfoType(List(), staticDefs, statics)

    clazz.setFlag(flags)
    clazz.setInfo(classInfo)
    statics.setFlag(Flags.JAVA)
    statics.setInfo(staticInfo)
    staticModule.setFlag(Flags.JAVA)
    staticModule.setInfo(statics.tpe)

    // import nested types
    for (ntype <- typ.getNestedTypes() if !(ntype.IsNestedPrivate
				            || ntype.IsNestedAssembly
				            || ntype.IsNestedFamANDAssem)
				          || ntype.IsInterface)
      {
	val loader = new loaders.MSILTypeLoader(ntype)
	val nclazz = statics.newClass(NoPosition, ntype.Name.toTypeName)
	val nmodule = statics.newModule(NoPosition, ntype.Name)
	nclazz.setInfo(loader)
	nmodule.setInfo(loader)
	staticDefs.enter(nclazz)
	staticDefs.enter(nmodule)

	assert(nclazz.companionModule == nmodule, nmodule)
	assert(nmodule.companionClass == nclazz, nclazz)
      }

    val fields = typ.getFields()
    for (field <- fields if !(field.IsPrivate() || field.IsAssembly() || field.IsFamilyAndAssembly)) {
      val flags = translateAttributes(field);
      val name = newTermName(field.Name);
      val fieldType =
        if (field.IsLiteral && !field.FieldType.IsEnum)
	  ConstantType(getConstant(getCLRType(field.FieldType), field.getValue))
	else getCLRType(field.FieldType);
      val owner = if (field.IsStatic()) statics else clazz;
      val sym = owner.newValue(NoPosition, name).setFlag(flags).setInfo(fieldType);
        // TODO: set private within!!! -> look at typechecker/Namers.scala
        (if (field.IsStatic()) staticDefs else instanceDefs).enter(sym);
      clrTypes.fields(sym) = field;
    }

    for (constr <- typ.getConstructors() if !constr.IsStatic() && !constr.IsPrivate() &&
         !constr.IsAssembly() && !constr.IsFamilyAndAssembly())
      createMethod(constr);

    // initially also contains getters an setters of properties.
    val methodsSet = new HashSet[MethodInfo]();
    methodsSet ++= typ.getMethods();

    for (prop <- typ.getProperties) {
      val propType: Type = getCLSType(prop.PropertyType);
      if (propType != null) {
	val getter: MethodInfo = prop.GetGetMethod(true);
	val setter: MethodInfo = prop.GetSetMethod(true);
	var gparamsLength: Int = -1;
	if (!(getter == null || getter.IsPrivate || getter.IsAssembly
	      || getter.IsFamilyAndAssembly))
	  {
	    assert(prop.PropertyType == getter.ReturnType);
	    val gparams: Array[ParameterInfo] = getter.GetParameters();
	    gparamsLength = gparams.length;
	    val name: Name = if (gparamsLength == 0) prop.Name else nme.apply;
	    val flags = translateAttributes(getter);
	    val owner: Symbol = if (getter.IsStatic) statics else clazz;
	    val methodSym = owner.newMethod(NoPosition, name).setFlag(flags)
	    val mtype: Type = if (gparamsLength == 0) PolyType(List(), propType)
                              else methodType(getter, getter.ReturnType)(methodSym)
        methodSym.setInfo(mtype);
	    methodSym.setFlag(Flags.ACCESSOR);
	    (if (getter.IsStatic) staticDefs else instanceDefs).enter(methodSym)
	    clrTypes.methods(methodSym) = getter;
	    methodsSet -= getter;
	  }
	if (!(setter == null || setter.IsPrivate || setter.IsAssembly
	     || setter.IsFamilyAndAssembly))
	  {
	    val sparams: Array[ParameterInfo] = setter.GetParameters()
	    if(getter != null)
	      assert(getter.IsStatic == setter.IsStatic);
	    assert(setter.ReturnType == clrTypes.VOID);
	    if(getter != null)
	      assert(sparams.length == gparamsLength + 1, "" + getter + "; " + setter);

	    val name: Name = if (gparamsLength == 0) nme.getterToSetter(prop.Name)
			     else nme.update;
	    val flags = translateAttributes(setter);
	    val mtype = methodType(setter, definitions.UnitClass.tpe);
	    val owner: Symbol = if (setter.IsStatic) statics else clazz;
	    val methodSym = owner.newMethod(NoPosition, name).setFlag(flags)
        methodSym.setInfo(mtype(methodSym))
	    methodSym.setFlag(Flags.ACCESSOR);
	    (if (setter.IsStatic) staticDefs else instanceDefs).enter(methodSym);
	    clrTypes.methods(methodSym) = setter;
	    methodsSet -= setter;
	  }
      }
    }

/*    for (event <- typ.GetEvents) {
      // adding += and -= methods to add delegates to an event.
      // raising the event ist not possible from outside the class (this is so
      // generally in .net world)
      val adder: MethodInfo = event.GetAddMethod();
      val remover: MethodInfo = event.GetRemoveMethod();
      if (!(adder == null || adder.IsPrivate || adder.IsAssembly
	    || adder.IsFamilyAndAssembly))
	{
	  assert(adder.ReturnType == clrTypes.VOID);
	  assert(adder.GetParameters().map(_.ParameterType).toList == List(event.EventHandlerType));
	  val name = encode("+=");
	  val flags = translateAttributes(adder);
	  val mtype: Type = methodType(adder, adder.ReturnType);
	  createMethod(name, flags, mtype, adder, adder.IsStatic)
	  methodsSet -= adder;
	}
      if (!(remover == null || remover.IsPrivate || remover.IsAssembly
	    || remover.IsFamilyAndAssembly))
	{
	  assert(remover.ReturnType == clrTypes.VOID);
	  assert(remover.GetParameters().map(_.ParameterType).toList == List(event.EventHandlerType));
	  val name = encode("-=");
	  val flags = translateAttributes(remover);
	  val mtype: Type = methodType(remover, remover.ReturnType);
	  createMethod(name, flags, mtype, remover, remover.IsStatic)
	  methodsSet -= remover;
	}
    } */

    for (method <- methodsSet.iterator)
      if (!method.IsPrivate() && !method.IsAssembly() && !method.IsFamilyAndAssembly())
        createMethod(method);

    // Create methods and views for delegate support
    if (clrTypes.isDelegateType(typ)) {
      createDelegateView(typ)
      createDelegateChainers(typ)
    }

    // create the box/unbox methods for value types
    if (typ.IsValueType) {
      val box = statics.newMethod(NoPosition, nme.box)
      box.setInfo(MethodType(box.newSyntheticValueParams(List(clazz.tpe)), definitions.ObjectClass.tpe))
      definitions.boxMethod(clazz) = box
      val unbox = statics.newMethod(NoPosition, nme.unbox)
      unbox.setInfo(MethodType(unbox.newSyntheticValueParams(List(definitions.ObjectClass.tpe)), clazz.tpe))
      definitions.unboxMethod(clazz) = unbox
      //Console.println(typ.FullName + " : " + parents)
    }

    // for enumerations introduce comparison and bitwise logical operations;
    // the backend should recognize and replace them with comparison or
    // bitwise logical operations on the primitive underlying type

    if (typ.IsEnum) {
      val ENUM_CMP_NAMES = List(nme.EQ, nme.NE, nme.LT, nme.LE, nme.GT, nme.GE);
      val ENUM_BIT_LOG_NAMES = List(nme.OR, nme.AND, nme.XOR);

      val flags = Flags.JAVA | Flags.FINAL
      for (cmpName <- ENUM_CMP_NAMES) {
        val enumCmp = clazz.newMethod(NoPosition, cmpName)
        val enumCmpType = JavaMethodType(enumCmp.newSyntheticValueParams(List(clazz.tpe)), definitions.BooleanClass.tpe)
        enumCmp.setFlag(flags).setInfo(enumCmpType)
        instanceDefs.enter(enumCmp)
      }

      for (bitLogName <- ENUM_BIT_LOG_NAMES) {
        val enumBitLog = clazz.newMethod(NoPosition, bitLogName)
        val enumBitLogType = JavaMethodType(enumBitLog.newSyntheticValueParams(List(clazz.tpe)), classInfo)
        enumBitLog.setFlag(flags).setInfo(enumBitLogType)
        instanceDefs.enter(enumBitLog)
      }
    }

  } // parseClass

  private def createMethod(method: MethodBase) {
    val rettype = if (method.IsConstructor()) clazz.tpe
                  else getCLSType(method.asInstanceOf[MethodInfo].ReturnType);
    if (rettype == null) return;
    val mtype = methodType(method, rettype);
    if (mtype == null) return;
    val flags = translateAttributes(method);
    val owner = if (method.IsStatic()) statics else clazz;
    val methodSym = owner.newMethod(NoPosition, getName(method)).setFlag(flags)
    methodSym.setInfo(mtype(methodSym))
    (if (method.IsStatic()) staticDefs else instanceDefs).enter(methodSym);
    if (method.IsConstructor())
      clrTypes.constructors(methodSym) = method.asInstanceOf[ConstructorInfo]
    else clrTypes.methods(methodSym) = method.asInstanceOf[MethodInfo];
  }

  private def createMethod(name: Name, flags: Long, args: Array[MSILType], retType: MSILType, method: MethodInfo, statik: Boolean): Symbol = {
    val mtype = methodType(args, getCLSType(retType))
    assert(mtype != null)
    createMethod(name, flags, mtype, method, statik)
  }

  private def createMethod(name: Name, flags: Long, mtype: Symbol => Type, method: MethodInfo, statik: Boolean): Symbol = {
    val methodSym: Symbol = (if (statik)  statics else clazz).newMethod(NoPosition, name)
    methodSym.setFlag(flags).setInfo(mtype(methodSym))
    (if (statik) staticDefs else instanceDefs).enter(methodSym)
    if (method != null)
      clrTypes.methods(methodSym)  = method
    methodSym
  }

  private def createDelegateView(typ: MSILType) = {
    val invoke: MethodInfo = typ.GetMember("Invoke")(0).asInstanceOf[MethodInfo];
    val invokeRetType: Type = getCLRType(invoke.ReturnType);
    val invokeParamTypes: List[Type] =invoke.GetParameters().map(_.ParameterType).map(getCLSType).toList;
    val funType: Type = definitions.functionType(invokeParamTypes, invokeRetType);

    val typClrType: Type = getCLRType(typ);
    val flags = Flags.JAVA | Flags.STATIC | Flags.IMPLICIT; // todo: static? think not needed

    // create the forward view: delegate => function
    val delegateParamTypes: List[Type] = List(typClrType);
    // not ImplicitMethodType, this is for methods with implicit parameters (not implicit methods)
    val forwardViewMethodType = (msym: Symbol) => JavaMethodType(msym.newSyntheticValueParams(delegateParamTypes), funType)
    val fmsym = createMethod(nme.view_, flags, forwardViewMethodType, null, true);

    // create the backward view: function => delegate
    val functionParamTypes: List[Type] = List(funType);
    val backwardViewMethodType = (msym: Symbol) => JavaMethodType(msym.newSyntheticValueParams(functionParamTypes), typClrType)
    val bmsym = createMethod(nme.view_, flags, backwardViewMethodType, null, true);
  }

  private def createDelegateChainers(typ: MSILType) = {
    val flags: Long = Flags.JAVA | Flags.FINAL
    val args: Array[MSILType] = Array(typ)

    var s = createMethod(encode("+="), flags, args, clrTypes.VOID, clrTypes.DELEGATE_COMBINE, false);
    s = createMethod(encode("-="), flags, args, clrTypes.VOID, clrTypes.DELEGATE_REMOVE, false);

    s = createMethod(nme.PLUS, flags, args, typ, clrTypes.DELEGATE_COMBINE, false);
    s = createMethod(nme.MINUS, flags, args, typ, clrTypes.DELEGATE_REMOVE, false);
  }

  private def getName(method: MethodBase): Name = {

    def operatorOverload(name : String, paramsArity : Int) : Option[Name] = paramsArity match {
      case 1 => name match {
        // PartitionI.10.3.1
        case "op_Decrement" => Some(encode("--"))
        case "op_Increment" => Some(encode("++"))
        case "op_UnaryNegation" => Some(nme.UNARY_-)
        case "op_UnaryPlus" => Some(nme.UNARY_+)
        case "op_LogicalNot" => Some(nme.UNARY_!)
        case "op_OnesComplement" => Some(nme.UNARY_~)
        /* op_True and op_False have no operator symbol assigned,
           Other methods that will have to be written in full are:
           op_AddressOf & (unary)
           op_PointerDereference * (unary) */
        case _ => None
      }
      case 2 => name match {
        // PartitionI.10.3.2
        case "op_Addition" => Some(nme.ADD)
        case "op_Subtraction" => Some(nme.SUB)
        case "op_Multiply" => Some(nme.MUL)
        case "op_Division" => Some(nme.DIV)
        case "op_Modulus" => Some(nme.MOD)
        case "op_ExclusiveOr" => Some(nme.XOR)
        case "op_BitwiseAnd" => Some(nme.AND)
        case "op_BitwiseOr" => Some(nme.OR)
        case "op_LogicalAnd" => Some(nme.ZAND)
        case "op_LogicalOr" => Some(nme.ZOR)
        case "op_LeftShift" => Some(nme.LSL)
        case "op_RightShift" => Some(nme.ASR)
        case "op_Equality" => Some(nme.EQ)
        case "op_GreaterThan" => Some(nme.GT)
        case "op_LessThan" => Some(nme.LT)
        case "op_Inequality" => Some(nme.NE)
        case "op_GreaterThanOrEqual" => Some(nme.GE)
        case "op_LessThanOrEqual" => Some(nme.LE)

        /* op_MemberSelection is reserved in Scala  */

        /* The standard does not assign operator symbols to op_Assign , op_SignedRightShift , op_UnsignedRightShift ,
         *   and op_UnsignedRightShiftAssignment so those names will be used instead to invoke those methods. */

        /*
          The remaining binary operators are not overloaded in C# and are therefore not in widespread use. They have to be written in full.

          op_RightShiftAssignment      >>=
          op_MultiplicationAssignment  *=
          op_PointerToMemberSelection  ->*
          op_SubtractionAssignment     -=
          op_ExclusiveOrAssignment     ^=
          op_LeftShiftAssignment       <<=
          op_ModulusAssignment         %=
          op_AdditionAssignment        +=
          op_BitwiseAndAssignment      &=
          op_BitwiseOrAssignment       |=
          op_Comma                     ,
          op_DivisionAssignment        /=
        */
        case _ => None
      }
      case _ => None
    }

    if (method.IsConstructor()) return nme.CONSTRUCTOR;
    val name = method.Name;
    if (method.IsStatic()) {
      if(method.IsSpecialName) {
        val paramsArity = method.GetParameters().size
        // handle operator overload, otherwise handle as any static method
        val operName = operatorOverload(name, paramsArity)
        if (operName.isDefined) { return operName.get; }
      }
      return newTermName(name);
    }
    val params = method.GetParameters();
    name match {
      case "GetHashCode" if (params.length == 0) => nme.hashCode_;
      case "ToString" if (params.length == 0) => nme.toString_;
      case "Finalize" if (params.length == 0) => nme.finalize_;
      case "Equals" if (params.length == 1 && params(0).ParameterType == clrTypes.OBJECT) =>
        nme.equals_;
      case "Invoke" if (clrTypes.isDelegateType(method.DeclaringType)) => nme.apply;
      case _ => newTermName(name);
    }
  }

  //##########################################################################

  private def methodType(method: MethodBase, rettype: MSILType): Symbol => Type = {
    val rtype = getCLSType(rettype);
    if (rtype == null) null else methodType(method, rtype);
  }

  /** Return a method type for the given method. */
  private def methodType(method: MethodBase, rettype: Type): Symbol => Type =
    methodType(method.GetParameters().map(_.ParameterType), rettype);

  /** Return a method type for the provided argument types and return type. */
  private def methodType(argtypes: Array[MSILType], rettype: Type): Symbol => Type = {
    def paramType(typ: MSILType): Type =
      if (typ eq clrTypes.OBJECT) definitions.AnyClass.tpe
      else getCLSType(typ);
    val ptypes = argtypes.map(paramType).toList;
    if (ptypes.contains(null)) null
    else method => JavaMethodType(method.newSyntheticValueParams(ptypes), rettype);
  }

    //##########################################################################

  private def getClassType(typ: MSILType): Type = {
    assert(typ != null);
    val res = definitions.getClass(typ.FullName.replace('+', '.')).tpe;
    //if (res.isError())
    //  global.reporter.error("unknown class reference " + type.FullName);
    res
  }

  private def getCLSType(typ: MSILType): Type = {
    if (/*type == clrTypes.BYTE ||*/ typ == clrTypes.USHORT
	|| typ == clrTypes.UINT || typ == clrTypes.ULONG
	|| typ.IsNotPublic() || typ.IsNestedPrivate()
	|| typ.IsNestedAssembly() || typ.IsNestedFamANDAssem()
	|| typ.IsPointer()
	|| (typ.IsArray() && getCLSType(typ.GetElementType()) == null))
      null;
    //Symbol s = clrTypes.getSymbol(type);
    //scalac.symtab.Type t = s != null ? make.classType(s) : getCLRType(type);
    else
      getCLRType(typ)
  }

  private def getCLRType(typ: MSILType): Type =
    if (typ == clrTypes.OBJECT)
      definitions.ObjectClass.tpe;
    else if (typ == clrTypes.VALUE_TYPE)
      definitions.AnyValClass.tpe
    else if (typ == clrTypes.STRING)
      definitions.StringClass.tpe;
    else if (typ == clrTypes.VOID)
      definitions.UnitClass.tpe
    else if (typ == clrTypes.BOOLEAN)
      definitions.BooleanClass.tpe
    else if (typ == clrTypes.CHAR)
      definitions.CharClass.tpe
    else if (typ == clrTypes.BYTE || typ == clrTypes.UBYTE)
      definitions.ByteClass.tpe
    else if (typ == clrTypes.SHORT || typ == clrTypes.USHORT)
      definitions.ShortClass.tpe
    else if (typ == clrTypes.INT || typ == clrTypes.UINT)
      definitions.IntClass.tpe
    else if (typ == clrTypes.LONG || typ == clrTypes.ULONG)
      definitions.LongClass.tpe
    else if (typ == clrTypes.FLOAT)
      definitions.FloatClass.tpe
    else if (typ == clrTypes.DOUBLE)
      definitions.DoubleClass.tpe
    else if (typ.IsArray())
      appliedType(definitions.ArrayClass.tpe,
                  List(getCLRType(typ.GetElementType())));
    else if (typ.isInstanceOf[ConstructedType]) {
      val ct = typ.asInstanceOf[ConstructedType]
      getCLRType(ct.instantiatedType)
    } else {
      val res = clrTypes.sym2type.get (typ) match {
        case Some(sym) => sym.tpe
        case None => getClassType(typ);
      }
      assert (res != null, typ)
      res
    }

  // the values are Java-Box-Classes (e.g. Integer, Boolean, Character)
  // java.lang.Number to get the value (if a number, not for boolean, character)
  // see ch.epfl.lamp.compiler.msil.util.PEStream.java
  def getConstant(constType: Type, value: Object): Constant = {
    val typeClass = constType.typeSymbol
    if (typeClass == definitions.BooleanClass)
      Constant(value.asInstanceOf[java.lang.Boolean].booleanValue)
    else if (typeClass == definitions.ByteClass)
      Constant(value.asInstanceOf[java.lang.Number].byteValue)
    else if (typeClass == definitions.ShortClass)
      Constant(value.asInstanceOf[java.lang.Number].shortValue)
    else if (typeClass == definitions.CharClass)
      Constant(value.asInstanceOf[java.lang.Character].charValue)
    else if (typeClass == definitions.IntClass)
      Constant(value.asInstanceOf[java.lang.Number].intValue)
    else if (typeClass == definitions.LongClass)
      Constant(value.asInstanceOf[java.lang.Number].longValue)
    else if (typeClass == definitions.FloatClass)
      Constant(value.asInstanceOf[java.lang.Number].floatValue)
    else if (typeClass == definitions.DoubleClass)
      Constant(value.asInstanceOf[java.lang.Number].doubleValue)
    else if (typeClass == definitions.StringClass)
      Constant(value.asInstanceOf[java.lang.String])
    else
      abort("illegal value: " + value + ", class-symbol: " + typeClass)
  }

  private def translateAttributes(typ: MSILType): Long = {
    var flags: Long = Flags.JAVA;
    if (typ.IsNotPublic() || typ.IsNestedPrivate()
	|| typ.IsNestedAssembly() || typ.IsNestedFamANDAssem())
      flags = flags | Flags.PRIVATE;
    else if (typ.IsNestedFamily() || typ.IsNestedFamORAssem())
      flags = flags | Flags.PROTECTED;
    if (typ.IsAbstract())
      flags = flags | Flags.ABSTRACT;
    if (typ.IsSealed())
      flags = flags | Flags.FINAL;
    if (typ.IsInterface())
      flags = flags | Flags.INTERFACE | Flags.TRAIT | Flags.ABSTRACT;

    flags
  }

  private def translateAttributes(field: FieldInfo): Long = {
    var flags: Long = Flags.JAVA;
    if (field.IsPrivate() || field.IsAssembly() || field.IsFamilyAndAssembly())
      flags = flags | Flags.PRIVATE;
    else if (field.IsFamily() || field.IsFamilyOrAssembly())
      flags = flags | Flags.PROTECTED;
    if (field.IsInitOnly())
      flags = flags | Flags.FINAL;
    else
      flags = flags | Flags.MUTABLE;
    if (field.IsStatic)
      flags = flags | Flags.STATIC

    flags
  }

  private def translateAttributes(method: MethodBase): Long = {
    var flags: Long = Flags.JAVA;
    if (method.IsPrivate() || method.IsAssembly() || method.IsFamilyAndAssembly())
      flags = flags | Flags.PRIVATE;
    else if (method.IsFamily() || method.IsFamilyOrAssembly())
      flags = flags | Flags.PROTECTED;
    if (method.IsAbstract())
      flags = flags | Flags.DEFERRED;
    if (method.IsStatic)
      flags = flags | Flags.STATIC

    flags
  }
}
