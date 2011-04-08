package ch.epfl.lamp.compiler.msil;

/**
 * Quoting from  the CIL spec, Partition II, Sec. 7.1.1:
 *
 * Custom modifiers, defined using `modreq` (required modifier) and `modopt` (optional modifier), are
 * similar to custom attributes (Sec. 21) except that modifiers are part of a signature rather than being attached to a
 * declaration. Each modifer associates a type reference with an item in the signature.
 *
 */
public class CustomModifier {

    public boolean isReqd;
    public Type    marker;

    public CustomModifier(boolean isReqd, Type marker) {
        this.isReqd = isReqd;
        this.marker = marker;
    }

    public String toString() {
        String res = (isReqd ? "modreq( " : "modopt( ") + marker.toString() + " )";
        return res;
    }

    public static Type[] helperCustomMods(boolean isReqd, CustomModifier[] cmods) {
        if(cmods == null) return null;
        int count = 0;
        for (int idx = 0; idx < cmods.length; idx++) {
            if(cmods[idx].isReqd == isReqd) count++;
        }
        Type[] res = new Type[count];
        int residx = 0;
        for (int idx = 0; idx < cmods.length; idx++) {
            res[residx] = cmods[idx].marker;
            residx++;
        }
        return res;
    }

    public static Type VolatileMarker() {
        return Type.GetType("System.Runtime.CompilerServices.IsVolatile");
    }

}
