/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.symtab.classfile;

import scala.tools.util.AbstractFile;
import scalac.*;
import scalac.symtab.*;
import scalac.util.*;
import java.io.*;


public class SymblParser extends ClassParser {

    public SymblParser(Global global) {
	super(global);
    }

    /** complete class symbol c by loading the class
     */
    public String doComplete(Symbol clasz) throws IOException {
        AbstractFile file = global.classPath.openFile(
            SourceRepresentation.externalizeFileName(clasz, ".symbl"));
        UnPickle.parse(global, file, clasz);
        return "symbol file '" + file + "'";
    }
}
