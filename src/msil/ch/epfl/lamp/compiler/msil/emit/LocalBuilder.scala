/**
 * System.Reflection.Emit-like API for writing .NET assemblies to MSIL
 */


package ch.epfl.lamp.compiler.msil.emit

import ch.epfl.lamp.compiler.msil.Type

/**
 * Represents a local variable within a method or constructor.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
class LocalBuilder(_slot : Int, localType : Type) extends Visitable {

    /**
     * the type of the local variable.
     */
    var LocalType : Type = localType

    // the name of the local variable
    var name : String = "L_" + slot

    // the slot occupied by this local in the corresponding ILGenerator
    var slot : Int = _slot

    /**
     * Sets the name of this local variable.
     */
    def SetLocalSymInfo(name : String) {
	    this.name = name
    }

    override def toString() : String = name

    /**
     * the apply method for a visitor
     */
    def apply(v : Visitor) {
	    v.caseLocalBuilder(this)
    }
}
