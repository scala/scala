/*
 * System.Reflection.Emit-like API for writing .NET assemblies to MSIL
 */


package ch.epfl.lamp.compiler.msil.emit

import ch.epfl.lamp.compiler.msil.ConstructorInfo

/**
 * Declares the possibility to set a custom attribute for a member
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
trait ICustomAttributeSetter {
    def SetCustomAttribute(constr: ConstructorInfo, value: Array[Byte])
}
