/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.symtab;

import ch.epfl.lamp.compiler.msil.Assembly;
import scala.tools.util.AbstractFile;
import scalac.CompilationUnit;

/** Instances of this class designate the origin of a symbol. */
public class SymbolOrigin {

    //########################################################################
    // Public Cases

    /** Designates an unknown source */
    public case Unknown;

    /** Designates a directory */
    public case Directory(AbstractFile file);

    /** Designates a JVM class file (the source file may be null) */
    public case ClassFile(AbstractFile file, String sourcefile);

    /** Designates a Scala symbl file */
    public case SymblFile(AbstractFile file);

    /** Designates a Scala source file */
    public case ScalaFile(AbstractFile file);

    /** Designates a Scala compilation unit */
    public case ScalaUnit(CompilationUnit unit);

    /** Designates a CLR assembly */
    public case CLRAssembly(Assembly assembly);

    //########################################################################
    // Public Methods

    /** Records the source file attribute. */
    public void setSourceFileAttribute(String sourcefile) {
        if (this instanceof SymbolOrigin.ClassFile)
            ((SymbolOrigin.ClassFile)this).sourcefile = sourcefile;
    }

    //########################################################################
}
