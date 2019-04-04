package jkson;

abstract class Erased { public abstract Object foo(); }
public abstract class Generic<T> extends Erased { public T foo() { return null; } }
