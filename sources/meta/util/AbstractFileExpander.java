/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package meta.util;

import java.io.File;

/** A base class for file expanders. */
public abstract class AbstractFileExpander {

    //########################################################################
    // Private Constants

    /** The meta package */
    private static final String meta = "meta";

    /** The meta name prefix */
    private static final String Meta = "Meta";

    //########################################################################
    // Public Methods

    /** Returns the TextWriter in which this expander writes. */
    public abstract TextWriter getTextWriter();

    /**
     * Returns the package associated with this expander or null if
     * there no such package. The default implementation returns the
     * package of this instance's class. If the outermost package is
     * named "meta", this package is omitted.
     */
    public String getPackage() {
        String fullname = getClass().getName();
        int end = fullname.lastIndexOf('.');
        if (end < 0) return null;
        int start = fullname.startsWith(meta + ".") ? meta.length() + 1 : 0;
        return fullname.substring(start, end);
    }

    /**
     * Returns the name associated with this expander. The default
     * implementation returns the name of this instance's class. If
     * that name starts with "Meta", this prefix is omitted.
     */
    public String getName() {
        String fullname = getClass().getName();
        int index = fullname.lastIndexOf('.');
        String name = index < 0 ? fullname : fullname.substring(index + 1);
        return name.startsWith(Meta) ? name.substring(Meta.length()) : name;
    }

    /**
     * Returns the directory of the target file. The default
     * implementation returns the directory corresponding to the
     * this instance's associated package.
     */
    public String getTargetDirectory() {
        String peckage = getPackage();
        return peckage == null ? "." : peckage.replace('.',File.separatorChar);
    }

    /**
     * Returns the base name of the target file. The default
     * implementation returns this instance's associated name.
     */
    public String getTargetBaseName() {
        return getName();
    }

    /**
     * Returns the suffix of the target file or null if it has no
     * suffix. The default implementation returns null.
     */
    public String getTargetSuffix() {
        return null;
    }

    /** Returns the target file. */
    public File getTargetFile(File root) {
        String suffix = getTargetSuffix();
        String name = getTargetBaseName();
        if (suffix != null) name = name + "." + suffix;
        return new File(new File(root, getTargetDirectory()), name);
    }

    /**
     * Returns the directory of the source file. The default
     * implementation returns the directory of the target file.
     */
    public String getSourceDirectory() {
        return getTargetDirectory();
    }

    /**
     * Returns the base name of the source file. The default
     * implementation returns the target base name.
     */
    public String getSourceBaseName() {
        return getTargetBaseName();
    }

    /**
     * Returns the suffix of the source file or null if it has no
     * suffix. The default implementation returns the target suffix
     * suffixed with ".tmpl".
     */
    public String getSourceSuffix() {
        String suffix = getTargetSuffix();
        return (suffix == null ? "" : suffix + ".") + "tmpl";
    }

    /** Returns the source file. */
    public File getSourceFile(File root) {
        String suffix = getSourceSuffix();
        String name = getSourceBaseName();
        if (suffix != null) name = name + "." + suffix;
        return new File(new File(root, getSourceDirectory()), name);
    }

    //########################################################################
}
