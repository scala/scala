/* FJBG -- Fast Java Bytecode Generator
 * Copyright 2002-2011 LAMP/EPFL
 * @author  Michel Schinz
 */

package ch.epfl.lamp.fjbg;

/**
 * Types for Java references, i.e. arrays and objects.
 *
 * @author Michel Schinz
 * @version 1.0
 */

abstract public class JReferenceType extends JType {
    public boolean isReferenceType() { return true; }

    abstract public String getDescriptor();
}
