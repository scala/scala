/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.runtime.types;

import scala.Type;
import scala.Array;

import java.util.HashMap;
import java.util.Map;

public class JavaTypeRepository {
//     public static JavaClassType DOUBLE = new JavaClassType(double.class);
//     public static JavaClassType FLOAT  = new JavaClassType(float.class);
//     public static JavaClassType LONG   = new JavaClassType(long.class);
//     public static JavaClassType INT    = new JavaClassType(int.class);
//     public static JavaClassType SHORT  = new JavaClassType(short.class);
//     public static JavaClassType CHAR   = new JavaClassType(char.class);
//     public static JavaClassType BYTE   = new JavaClassType(byte.class);
//     public static JavaClassType VOID   = new JavaClassType(void.class);

    private static Map/*<String,JavaClassType>*/ map = new HashMap();

    private static final ClassLoader loader =
        ClassLoader.getSystemClassLoader();

    // TODO utiliser des hash
    public static JavaClassType get(String name) {
        JavaClassType jt;
        synchronized (map) { jt = (JavaClassType)map.get(name); }

        if (jt == null) {
            try {
                jt = new JavaClassType(Class.forName(name, false, loader));
            } catch (ClassNotFoundException e) {
                throw new Error(e);
            }

            synchronized (map) {
                if (map.containsKey(name))
                    jt = (JavaClassType)map.get(name);
                else
                    map.put(name, jt);
            }
        }
        return jt;
    }
}
