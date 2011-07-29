package scala

package object reflect {

  val mirror: api.Mirror = try {
    // we use (Java) reflection here so that we can keep reflect.runtime and reflect.internals in a seperate jar
    (java.lang.Class forName "scala.reflect.runtime.Mirror$" getField "MODULE$" get null).asInstanceOf[api.Mirror]
  } catch {
    case ex: NoClassDefFoundError =>
      throw new UnsupportedOperationException("Scala reflection not available on this platform")
  }
  /** Uncomment once we got rid of the old Symbols, Types, Trees
  type Symbol = mirror.Symbol
  type Type = mirror.Type
  type Tree = mirror.Tree
  */

}
