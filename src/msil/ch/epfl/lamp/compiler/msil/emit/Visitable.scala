/*
 * System.Reflection.Emit-like API for writing .NET assemblies to MSIL
 */


package ch.epfl.lamp.compiler.msil.emit

import java.io.IOException

/**
 * The Visitable interface
 */
trait Visitable {

    //##########################################################################

    /**
     * the visitable method to apply a visitor
     */
    @throws(classOf[IOException])
    def apply(v: Visitor): Unit

    //##########################################################################
}
