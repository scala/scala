/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.atree;

/** This class represents a constant. */
public class AConstant {

    //########################################################################
    // Public Cases

    public case UNIT;
    public case BOOLEAN(boolean value);
    public case BYTE(byte value);
    public case SHORT(short value);
    public case CHAR(char value);
    public case INT(int value);
    public case LONG(long value);
    public case FLOAT(float value);
    public case DOUBLE(double value);
    public case STRING(String value);
    public case NULL;
    public case ZERO;

    //########################################################################
    // Public Methods

    /** Returns a string representation of this constant. */
    public String toString() {
        return new ATreePrinter().printConstant(this).toString();
    }

    //########################################################################
}
