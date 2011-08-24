package scala.reflect
package runtime

/**
 *  This symbol table trait fills in the definitions so that class information is obtained by refection.
 *  It can be used either from the reflexive mirror itself (class Universe), or else from
 *  a runtime compiler that uses reflection to get a class information (class scala.tools.nsc.ReflectGlobal)
 */
trait SymbolTable extends internal.SymbolTable with JavaToScala with ScalaToJava with Loaders
