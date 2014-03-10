package java.lang

// SI-8367 shows something is wrong with primaryConstructor and it was made worse with the fix for SI-8192
// perhaps primaryConstructor should not return NoSymbol when isJavaDefined
// or, perhaps isJavaDefined should be refined (the package definition above is pretty sneaky) 
// also, why does this only happen for a (scala-defined!) class with this special name?
// (there are a couple of others: CloneNotSupportedException,InterruptedException)
class Throwable

// class CloneNotSupportedException 
// class InterruptedException