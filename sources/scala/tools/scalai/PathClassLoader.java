/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: PathClassLoader.java,v 1.1 2002/10/01 16:09:34 paltherr Exp $
// $Id$

package scala.tools.scalai;

import java.io.IOException;

import scalac.util.ClassPath;
import scalac.util.AbstractFile;

public class PathClassLoader extends ClassLoader {

    //########################################################################
    // PathClassLoader state

    private final ClassPath classpath;

    //########################################################################
    // PathClassLoader constructors

    public PathClassLoader(ClassPath classpath) {
        this.classpath = classpath;
    }

    //########################################################################
    // ClassLoader interface

    public Class findClass(String name) throws ClassNotFoundException {
        try {
            String filename = name.replace('.', '/') + ".class";
            AbstractFile file = classpath.openFile(filename);
            byte[] bytes = file.read();
            return defineClass(name, bytes, 0, bytes.length);
        } catch (IOException exception) {
            throw new ClassNotFoundException(name);
        }
    }

    //########################################################################
}
