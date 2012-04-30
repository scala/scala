package scala.reflect.makro
package internal

/** This type is required by the compiler and <b>should not be used in client code</b>. */
class macroImpl(val referenceToMacroImpl: Any) extends annotation.StaticAnnotation
