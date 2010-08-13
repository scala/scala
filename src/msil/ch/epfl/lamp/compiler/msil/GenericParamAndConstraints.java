package ch.epfl.lamp.compiler.msil;

/**
 * @author Miguel Garcia
 */
public class GenericParamAndConstraints {

    public GenericParamAndConstraints(int Number, String Name, Type[] Constraints,
                                      boolean isInvariant, boolean isCovariant, boolean isContravariant,
                                      boolean isReferenceType, boolean isValueType, boolean hasDefaultConstructor) {
        this.Number = Number;
        this.Name = Name;
        this.Constraints = Constraints; // TODO representation for the class and new() constraints missing
        this.isInvariant = isInvariant;
        this.isCovariant = isCovariant;
        this.isContravariant = isContravariant;
        this.isReferenceType = isReferenceType;
        this.isValueType  = isValueType;
        this.hasDefaultConstructor = hasDefaultConstructor;

    }

    public final int Number;
    public final String Name; // can be null
    public final Type[] Constraints; // can be empty array
    public final boolean isInvariant; // only relevant for TVars, not for an MVar
    public final boolean isCovariant; // only relevant for TVars, not for an MVar
    public final boolean isContravariant; // only relevant for TVars, not for an MVar
    public final boolean isReferenceType;
    public final boolean isValueType;
    public final boolean hasDefaultConstructor;

    public String toString() {
        String res = Name == null ? "<NoName>" : (Name.equals("") ? "<NoName>" : Name);
        res = res + " <: " + Constraints;
        return res;
    }

}

