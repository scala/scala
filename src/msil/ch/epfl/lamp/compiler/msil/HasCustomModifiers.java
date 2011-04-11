package ch.epfl.lamp.compiler.msil;

public interface HasCustomModifiers {

    public Type[] GetOptionalCustomModifiers();

    public Type[] GetRequiredCustomModifiers();

}
