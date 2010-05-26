/*
 * System.Reflection.Emit-like API for writing .NET assemblies to MSIL
 */


package ch.epfl.lamp.compiler.msil.emit

import ch.epfl.lamp.compiler.msil.ConstructorInfo
import ch.epfl.lamp.compiler.msil.Type
import java.io.IOException

/**
 * Defines and represents a constructor of a dynamic class.
 * ConstructorBuilder is used to fully describe a constructor in
 * Microsoft intermediate language (MSIL), including the name, attributes,
 * signature, and constructor body. It is used in conjunction with the
 * TypeBuilder class to create classes at run time. Call DefineConstructor
 * to get an instance of ConstructorBuilder.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
class ConstructorBuilder(declType: Type, attrs: Int, paramTypes: Array[Type])
      extends ConstructorInfo(declType, attrs, paramTypes)
      with ICustomAttributeSetter
      with Visitable
{

    //##########################################################################
    // public interface

    /** Defines a parameter of this constructor. */
    def DefineParameter(pos: Int, attr: Int, name: String): ParameterBuilder = {
	val param = new ParameterBuilder(name, params(pos).ParameterType, attr, pos)
	params(pos) = param
	return param
    }

    /** Returns an ILGenerator for this constructor. */
    def GetILGenerator(): ILGenerator = {
	return ilGenerator
    }

    /** Sets a custom attribute. */
    def SetCustomAttribute(constr: ConstructorInfo, value: Array[Byte]) {
	addCustomAttribute(constr, value)
    }

    //##########################################################################

    /** The apply method for a visitor. */
    @throws(classOf[IOException])
    def apply(v: Visitor) {
	v.caseConstructorBuilder(this)
    }

    //##########################################################################

    // the Intermediate Language Generator
    // it contains the method's body
    protected var ilGenerator: ILGenerator = new ILGenerator(this)

    //##########################################################################
}
