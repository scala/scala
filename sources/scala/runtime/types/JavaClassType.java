/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.runtime.types;

import java.util.HashMap;

public class JavaClassType extends ClassType {
    private static final ClassLoader loader =
        ClassLoader.getSystemClassLoader();

    private static ThreadLocal cacheLocal = new ThreadLocal() {
            protected Object initialValue() {
                return new HashMap();
            }
        };

    public static JavaClassType javaClassType(String fullName) {
        HashMap/*<String, JavaClassType>*/ cache = (HashMap)cacheLocal.get();
        JavaClassType jct = (JavaClassType)cache.get(fullName);
        if (jct == null) {
            try {
                jct = new JavaClassType(fullName);
                cache.put(fullName, jct);
            } catch (ClassNotFoundException e) {
                throw new Error(e);
            }
        }
        return jct;
    }

    public static JavaClassType javaArrayType(String elemFullName, int dimen) {
        StringBuffer fullName = new StringBuffer();
        for (int i = 0; i < dimen; ++i)
            fullName.append('[');
        if (elemFullName.length() == 1 && "ZBCDFIJS".indexOf(elemFullName) >= 0)
            fullName.append(elemFullName);
        else
            fullName.append('L').append(elemFullName).append(';');
        return javaClassType(fullName.toString());
    }

    public JavaClassType(String fullName) throws ClassNotFoundException {
        super(Class.forName(fullName, false, loader), true);
    }
}
