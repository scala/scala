/*
 * System.Reflection.Emit-like API for writing .NET assemblies to MSIL
 */


package ch.epfl.lamp.compiler.msil.emit

import ch.epfl.lamp.compiler.msil.FieldInfo
import ch.epfl.lamp.compiler.msil.Type
import ch.epfl.lamp.compiler.msil.FieldAttributes
import ch.epfl.lamp.compiler.msil.ConstructorInfo

import ch.epfl.lamp.compiler.msil.util.PECustomMod

import java.io.IOException

/**
 * Discovers the attributes of a field and provides access to field metadata.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
class FieldBuilder(name: String, declType: Type, attrs: Int, fieldTypeWithMods: PECustomMod)
      extends FieldInfo(name, declType, attrs, fieldTypeWithMods, null)
      with ICustomAttributeSetter
      with Visitable
{

    //##########################################################################
    // public interface

    /** Sets a custom attribute. */
    def SetCustomAttribute(constr: ConstructorInfo, value: Array[Byte]) {
	addCustomAttribute(constr, value)
    }

    //##########################################################################

    /** the apply method for a visitor */
    @throws(classOf[IOException])
    def apply(v: Visitor) {
	v.caseFieldBuilder(this)
    }

    //##########################################################################

    protected var defaultValue: Object = _

    /** Sets the default value of this field. */
    def SetConstant(defaultValue: Object) {
        this.defaultValue = defaultValue
    }

    /** Specifies the field layout. */
    def SetOffset(iOffset: Int) {
	//this.fieldOffset = FieldAttributes.Offset.Value(iOffset)
    }

    //##########################################################################
}
