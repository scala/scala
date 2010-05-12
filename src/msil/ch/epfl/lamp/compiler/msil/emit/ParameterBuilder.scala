/*
 * System.Reflection.Emit-like API for writing .NET assemblies to MSIL
 */


package ch.epfl.lamp.compiler.msil.emit

import ch.epfl.lamp.compiler.msil.Type
import ch.epfl.lamp.compiler.msil.ConstructorInfo
import ch.epfl.lamp.compiler.msil.ParameterInfo
import java.io.IOException

/**
 * Creates or associates parameter information.
 * Parameter attributes need to consistent with the method signature.
 * If you specify Out attributes for a parameter, you should ensure that
 * the type of that method parameter is a ByRef type
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
class ParameterBuilder(name: String, tpe: Type, attr: Int, pos: Int)
      extends ParameterInfo(name, tpe, attr, pos)
      with ICustomAttributeSetter
      with Visitable
{

    //##########################################################################

    /** Sets a custom attribute. */
    def SetCustomAttribute(constr: ConstructorInfo, value: Array[Byte]) {
	addCustomAttribute(constr, value)
    }

    //##########################################################################

    /** The apply method for a visitor */
    @throws(classOf[IOException])
    def apply(v: Visitor) {
	v.caseParameterBuilder(this)
    }

    //##########################################################################
}
