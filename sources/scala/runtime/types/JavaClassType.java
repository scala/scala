/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.runtime.types;

public class JavaClassType extends ClassType {
    private static final ClassLoader loader =
        ClassLoader.getSystemClassLoader();

    public JavaClassType(String fullName) throws ClassNotFoundException {
        super(Class.forName(fullName, false, loader), true);
    }
}
