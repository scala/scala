/*
 * System.Reflection.Emit-like API for writing .NET assemblies to MSIL
 */

// $Id: Visitable.java 14655 2008-04-15 09:37:02Z lorch $

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
