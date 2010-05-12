
package ch.epfl.lamp.fjbg;

/**
 * Types for Java references, i.e. arrays and objects.
 *
 * @version 1.0
 * @author Michel Schinz
 */

abstract public class JReferenceType extends JType {
    public boolean isReferenceType() { return true; }

    abstract public String getDescriptor();
}
