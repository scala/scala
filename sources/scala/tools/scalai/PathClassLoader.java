/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: PathClassLoader.java,v 1.1 2002/10/01 16:09:34 paltherr Exp $
// $Id$

package scala.tools.scalai;

import java.io.File;
import java.io.IOException;

import scala.tools.util.AbstractFile;

public class PathClassLoader extends ClassLoader {

    //########################################################################
    // PathClassLoader state

    private final AbstractFile root;

    //########################################################################
    // PathClassLoader constructors

    public PathClassLoader(AbstractFile root) {
        this.root = root;
    }

    //########################################################################
    // ClassLoader interface

    public Class findClass(String name) throws ClassNotFoundException {
        String filename = name.replace('.', File.separatorChar) + ".class";
        AbstractFile file = root.lookupPath(filename, false);
        if (file == null) throw new ClassNotFoundException(name);
        try {
            byte[] bytes = file.read();
            return defineClass(name, bytes, 0, bytes.length);
        } catch (IOException exception) {
            throw new ClassFormatError(name+"(Failed to read file "+file+")");
        }
    }

    //########################################################################
}
