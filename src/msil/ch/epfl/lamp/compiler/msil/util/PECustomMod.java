package ch.epfl.lamp.compiler.msil.util;

import ch.epfl.lamp.compiler.msil.Type;
import ch.epfl.lamp.compiler.msil.CustomModifier;

/**
 * A PECustomMod holds the info parsed from metadata per the CustomMod production in Sec. 23.2.7, Partition II.
 * */
public final class PECustomMod {

    public final Type marked;
    public final CustomModifier[] cmods;

    /** Terminology:
        the CustomModifier(s) are markers,
        and the msil.Type is a type marked by those markers. */
    public PECustomMod(Type marked, CustomModifier[] cmods) {
      this.marked = marked;
      this.cmods = cmods;
    }

}

