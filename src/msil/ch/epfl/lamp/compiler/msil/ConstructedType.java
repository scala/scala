package ch.epfl.lamp.compiler.msil;

import java.util.Arrays;

/* The only reason for ConstructedType to extend Type is complying with existing code
  (e.g., caseFieldBuilder in ILPrinterVisitor) expecting a Type.
 */
public class ConstructedType extends Type {

    public final Type instantiatedType;
    public final Type[] typeArgs;

    public ConstructedType(Type instantiatedType, Type[] typeArgs) {
        super(instantiatedType.Module, instantiatedType.Attributes, "", null, null, null, instantiatedType.auxAttr /*AuxAttr.None*/ , null);
        this.instantiatedType = instantiatedType;
        this.typeArgs = typeArgs;
    }

    public String toString() {
        String res = instantiatedType.toString()  + "[";
        for (int i = 0; i < typeArgs.length; i++) {
            res = res + typeArgs[i].toString();
            if(i + 1 < typeArgs.length) {
                res = res + ", ";
            }
        }
        return res + "]";
    }


    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        ConstructedType that = (ConstructedType) o;

        if (!instantiatedType.equals(that.instantiatedType)) return false;
        if (!Arrays.equals(typeArgs, that.typeArgs)) return false;

        return true;
    }

    public int hashCode() {
        int result = instantiatedType.hashCode();
        result = 31 * result + Arrays.hashCode(typeArgs);
        return result;
    }
}
