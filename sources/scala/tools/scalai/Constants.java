/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: Constants.java,v 1.3 2002/07/09 08:35:02 paltherr Exp $
// $Id$

package scalai;

import scala.runtime.RunTime;

import scalac.symtab.Type;
import scalac.symtab.TypeTags;
import scalac.util.Debug;

public class Constants {

    //########################################################################
    // Public Constants - kinds

    // !!! use literal constants ?
    public static final int UNIT    = TypeTags.UNIT;
    public static final int BOOLEAN = TypeTags.BOOLEAN;
    public static final int BYTE    = TypeTags.BYTE;
    public static final int SHORT   = TypeTags.SHORT;
    public static final int INT     = TypeTags.INT;
    public static final int LONG    = TypeTags.LONG;
    public static final int FLOAT   = TypeTags.FLOAT;
    public static final int DOUBLE  = TypeTags.DOUBLE;
    public static final int CHAR    = TypeTags.CHAR;
    public static final int STRING  = TypeTags.STRING;
    public static final int OBJECT  = TypeTags.STRING + 1;

    //########################################################################
    // Public Methods - literal

    // !!! remove ?
    public Object literal(             ) { return RunTime.box  (     ); }
    public Object literal(boolean value) { return new Boolean  (value); }
    public Object literal(byte    value) { return new Byte     (value); }
    public Object literal(short   value) { return new Short    (value); }
    public Object literal(char    value) { return new Character(value); }
    public Object literal(int     value) { return new Integer  (value); }
    public Object literal(long    value) { return new Long     (value); }
    public Object literal(float   value) { return new Float    (value); }
    public Object literal(double  value) { return new Double   (value); }
    public Object literal(Object  value) { return               value ; }

    // !!! remove ?
    public Object literal(int kind, Object value) {
        switch (kind) {
        case UNIT   : return literal();
        case BOOLEAN: return literal(((Boolean)value).booleanValue());
        case BYTE   : return literal(((Number)value).byteValue());
        case SHORT  : return literal(((Number)value).shortValue());
        case INT    : return literal(((Number)value).intValue());
        case LONG   : return literal(((Number)value).longValue());
        case FLOAT  : return literal(((Number)value).floatValue());
        case DOUBLE : return literal(((Number)value).doubleValue());
        case CHAR   : return literal((char)((Number)value).intValue());
        case STRING : return literal(value);
        case OBJECT : return literal(value);
        default     : throw Debug.abort("illegal kind " + kind);
        }
    }

    //########################################################################
    // Public Methods - zero

    public Object zero(Type type) {
        switch (type) {
        case TypeRef(_, _, _)     : return literal(null);
        case UnboxedArrayType(_)  : return literal(null);
        case UnboxedType(int kind): return zero(kind);
        default: throw Debug.abort("illegal type", type);
        }
    }

    public Object zero(int kind) {
        switch (kind) {
        case UNIT   : return literal();
        case BOOLEAN: return literal(false);
        case BYTE   : return literal((byte)0);
        case SHORT  : return literal((short)0);
        case CHAR   : return literal((char)0);
        case INT    : return literal((int)0);
        case LONG   : return literal((long)0);
        case FLOAT  : return literal((float)0);
        case DOUBLE : return literal((double)0);
        default     : throw Debug.abort("illegal kind " + kind);
        }
    }

    //########################################################################
}
