/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.symtab.classfile;

import scalac.*;
import scalac.symtab.*;
import scalac.util.*;
import java.io.*;


public class ClassParser extends SymbolLoader {

    public ClassParser(Global global) {
        super(global);
    }

    protected String doComplete(Symbol clasz) throws IOException {
        AbstractFile file = global.classPath.openFile(
            SourceRepresentation.externalizeFileName(clasz, ".class"));
        new ClassfileParser(global,new AbstractFileReader(file),clasz).parse();
        return "class file '" + file.getPath() + "'";
    }

}

