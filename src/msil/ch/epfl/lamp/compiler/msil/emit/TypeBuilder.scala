/*
 * System.Reflection.Emit-like API for writing .NET assemblies to MSIL
 */


package ch.epfl.lamp.compiler.msil.emit

import ch.epfl.lamp.compiler.msil._
import java.util.HashMap
import java.util.ArrayList
import java.util.Iterator
import java.io.IOException

/**
 * Defines and creates new instances of classes during runtime.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
class TypeBuilder (module: Module, attributes: Int, fullName: String, baseType: Type, interfaces: Array[Type], declType: Type)
      extends Type(module, attributes, fullName, baseType, interfaces, declType, 0)
      with ICustomAttributeSetter
      with Visitable
{
    import TypeBuilder._

    //##########################################################################
    // public members

    /** 'Bakes' the type. */
    def CreateType(): Type = {
	fields = fieldBuilders.toArray(new Array[FieldInfo](fieldBuilders.size())).asInstanceOf[Array[FieldInfo]]
	methods = methodBuilders.toArray(new Array[MethodInfo](methodBuilders.size())).asInstanceOf[Array[MethodInfo]]
	constructors = constructorBuilders.toArray(new Array[ConstructorInfo](constructorBuilders.size())).asInstanceOf[Array[ConstructorInfo]]
	nestedTypes = nestedTypeBuilders.toArray(new Array[Type](nestedTypeBuilders.size())).asInstanceOf[Array[Type]]

	raw = false
	if (DeclaringType == null)
	    Module.asInstanceOf[ModuleBuilder].addType(this)
	return this
    }

    /**
     * Adds a new field to the class, with the given name,
     * attributes and field type.
     */
    def DefineField(name: String, `type`: Type, attrs: Short): FieldBuilder = {
	val field: FieldBuilder = new FieldBuilder(name, this, attrs, `type`)
	fieldBuilders.add(field)
	return field
    }

    /**
     * Adds a new method to the class, with the given name and
     * method signature.
     */
    def DefineMethod(name: String, attrs: Short, returnType: Type, paramTypes: Array[Type]): MethodBuilder = {
	val method = new MethodBuilder(name, this, attrs, returnType, paramTypes)
    val methods = methodBuilders.iterator()
    while(methods.hasNext()) {
        val m = methods.next().asInstanceOf[MethodInfo]
	    if (methodsEqual(m, method))
		throw new RuntimeException("["+ Assembly() +
                   "] Method has already been defined: " + m)
	}
	methodBuilders.add(method)
	return method
    }

    /**
     * Adds a new constructor to the class, with the given attributes
     * and signature.
     */
    def DefineConstructor(attrs: Short, callingConvention: Short, paramTypes: Array[Type]): ConstructorBuilder = {
	val constr = new ConstructorBuilder(this, attrs, paramTypes)
	constructorBuilders.add(constr)
	return constr
    }

    /**
     * Defines a nested type given its name.
     */
    def DefineNestedType(name: String, attributes: Int, baseType: Type, interfaces: Array[Type]): TypeBuilder = {
    val nested = nestedTypeBuilders.iterator()
    while(nested.hasNext()) {
        val nt = nested.next().asInstanceOf[TypeBuilder]
		if (nt.Name.equals(name)) {
		    val message = "Nested type " + name + " has already been defined: " + nt
		    throw new RuntimeException(message)
		}
	    }
	val t = new TypeBuilder(Module, attributes, name, baseType, interfaces, this)
	nestedTypeBuilders.add(t)
	return t
    }

    /** Get the field with the corresponding name. */
    override def GetField(name: String): FieldInfo = {
	testRaw(name)
	return super.GetField(name)
    }

    /** Get all fields of the current Type. */
    override def GetFields(): Array[FieldInfo] = {
	testRaw("<GetFields>")
	return super.GetFields()
    }

    /**
     * Searches for a public instance constructor whose parameters
     * match the types in the specified array.
     */
    override def GetConstructor(params: Array[Type]): ConstructorInfo = {
	testRaw(".ctor" + types2String(params))
	return super.GetConstructor(params)
    }

    /**
     * Returns all the public constructors defined for the current Type.
     */
    override def GetConstructors(): Array[ConstructorInfo] = {
	testRaw("<GetConstructors>")
	return super.GetConstructors()
    }

    /**
     * Searches for the specified public method whose parameters
     * match the specified argument types.
     */
    override def GetMethod(name: String, params: Array[Type]): MethodInfo = {
	testRaw(name + types2String(params))
	return super.GetMethod(name, params)
    }

    /** Returns all the public methods of the current Type. */
    override def GetMethods(): Array[MethodInfo] = {
	testRaw("<GetMethods>")
	return super.GetMethods()
    }

    /** Searches for the nested type with the specified name. */
    override def GetNestedType(name: String): Type = {
      testRaw(name)
      super.GetNestedType(name)
    }

    /** Returns all the types nested within the current Type. */
    override def GetNestedTypes(): Array[Type] = {
      testRaw("<GetNestedTypes>")
      super.GetNestedTypes()
    }

    /** Returns a Type object that represents a one-dimensional array of the current type */
    def MakeArrayType(): Type = {
      Type.mkArray(this, 1)
    }

    /** Sets a custom attribute. */
    def SetCustomAttribute(constr: ConstructorInfo, value: Array[Byte]) {
	addCustomAttribute(constr, value)
    }

    def setPosition(sourceLine: Int, sourceFilename: String) {
	this.sourceLine = sourceLine
	this.sourceFilename = sourceFilename
    }

    def setSourceFilepath(sourceFilepath: String) {
    this.sourceFilepath = sourceFilepath
    }

    //##########################################################################
    // protected members

    var sourceLine: Int = _
    var sourceFilename: String = _
    var sourceFilepath: String = _

    var fieldBuilders = new ArrayList[FieldBuilder]()
    var methodBuilders = new ArrayList[MethodBuilder]()
    var constructorBuilders = new ArrayList[ConstructorBuilder]()
    var nestedTypeBuilders = new ArrayList[TypeBuilder]()

    // shows if the type is 'raw', i.e. still subject to changes
    private var raw = true

    // throws an exception if the type is 'raw',
    // i.e. not finalized by call to CreateType
    protected def testRaw(member: String) {
	if (raw)
	    throw new RuntimeException("Not supported for TypeBuilder before CreateType(): " +
		 FullName + "::" + member)
    }

    //##########################################################################
    // public members not part of the Reflection.Emit.TypeBuilder interface.

    /** The apply method for a visitor. */
    @throws(classOf[IOException])
    def apply(v: Visitor) {
	v.caseTypeBuilder(this)
    }

    //##########################################################################

}  // class TypeBuilder

object TypeBuilder {
    def types2String(types: Array[Type]): String = {
    var s = new StringBuffer("(")
    for(val i <- 0 until types.length) {
        if (i > 0) s.append(", ")
        s.append(types(i))
    }
    s.append(")")
    return s.toString()
    }

    def methodsEqual(m1: MethodInfo, m2: MethodInfo): Boolean = {
    if (!m1.Name.equals(m2.Name))
        return false
    if (m1.ReturnType != m2.ReturnType)
        return false
    val p1 = m1.GetParameters()
    val p2 = m2.GetParameters()
    if (p1.length != p2.length)
        return false
    for(val i <- 0 until p1.length)
        if (p1(i).ParameterType != p2(i).ParameterType)
        return false
    return true
     }
}
