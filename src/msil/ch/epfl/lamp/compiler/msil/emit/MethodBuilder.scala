/*
 * System.Reflection.Emit-like API for writing .NET assemblies to MSIL
 */

// $Id: MethodBuilder.java 14655 2008-04-15 09:37:02Z lorch $

package ch.epfl.lamp.compiler.msil.emit

import ch.epfl.lamp.compiler.msil.MethodInfo
import ch.epfl.lamp.compiler.msil.ParameterInfo
import ch.epfl.lamp.compiler.msil.Type
import ch.epfl.lamp.compiler.msil.ConstructorInfo
import java.io.IOException

/**
 * Defines and represents a method of a dynamic class.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
class MethodBuilder(name: String, declType: Type, attrs: Int, returnType: Type, paramTypes: Array[Type])
      extends MethodInfo(name, declType, attrs, returnType, paramTypes)
      with ICustomAttributeSetter
      with Visitable
{

    //##########################################################################
    // public interface

    /** Defines a parameter of this method. TODO: Parameters are indexed staring
     *  from number 1 for the first parameter
     */
    def DefineParameter(pos: Int, attr: Int, name: String): ParameterBuilder = {
	val param = new ParameterBuilder(name, params(pos).ParameterType, attr, pos)
	params(pos) = param
	return param
    }

    /** Returns an ILGenerator for this method. */
    def GetILGenerator(): ILGenerator = {
	if (ilGenerator == null)
	    throw new RuntimeException
		("No code generator avaiable for this method: " + this)
	return ilGenerator
    }

    /** Sets a custom attribute. */
    def SetCustomAttribute(constr: ConstructorInfo, value: Array[byte]) {
	addCustomAttribute(constr, value)
    }

    //##########################################################################

    /** The apply method for a visitor. */
    @throws(classOf[IOException])
    def apply(v: Visitor) {
	v.caseMethodBuilder(this)
    }

    //##########################################################################

    // the Intermediate Language Generator
    // it contains the method's body
    protected final val ilGenerator : ILGenerator =
	  if (DeclaringType == null // global method
	      || !DeclaringType.IsInterface())
	      new ILGenerator(this)
	  else null

    //##########################################################################
}
