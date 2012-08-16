package scala.reflect
package base

abstract class MirrorOf[U <: base.Universe with Singleton] {
  /** .. */
  val universe: U

  /** .. */
  def RootClass: U#ClassSymbol
  def RootPackage: U#ModuleSymbol
  def EmptyPackageClass: U#ClassSymbol
  def EmptyPackage: U#ModuleSymbol

  /** The symbol corresponding to the globally accessible class with the
   *  given fully qualified name `fullName`.
   *
   *  If the name points to a type alias, it's recursively dealiased and its target is returned.
   *  If you need a symbol that corresponds to the type alias itself, load it directly from the package class:
   *
   *    scala> cm.staticClass("scala.List")
   *    res0: reflect.runtime.universe.ClassSymbol = class List
   *
   *    scala> res0.fullName
   *    res1: String = scala.collection.immutable.List
   *
   *    scala> cm.staticPackage("scala")
   *    res2: reflect.runtime.universe.ModuleSymbol = package scala
   *
   *    scala> res2.moduleClass.typeSignature member newTypeName("List")
   *    res3: reflect.runtime.universe.Symbol = type List
   *
   *    scala> res3.fullName
   *    res4: String = scala.List
   *
   *  To be consistent with Scala name resolution rules, in case of ambiguity between
   *  a package and an object, the object is never been considered.
   *
   *  For example for the following code:
   *
   *    package foo {
   *      class B
   *    }
   *
   *    object foo {
   *      class A
   *      class B
   *    }
   *
   *  staticClass("foo.B") will resolve to the symbol corresponding to the class B declared in the package foo, and
   *  staticClass("foo.A") will throw a MissingRequirementException (which is exactly what scalac would do if this
   *  fully qualified class name is written inside any package in a Scala program).
   *
   *  In the example above, to load a symbol that corresponds to the class B declared in the object foo,
   *  use staticModule("foo") to load the module symbol and then navigate typeSignature.members of its moduleClass.
   */
  def staticClass(fullName: String): U#ClassSymbol

  /** The symbol corresponding to the globally accessible object with the
   *  given fully qualified name `fullName`.
   *
   *  To be consistent with Scala name resolution rules, in case of ambiguity between
   *  a package and an object, the object is never been considered.
   *
   *  For example for the following code:
   *
   *    package foo {
   *      object B
   *    }
   *
   *    object foo {
   *      object A
   *      object B
   *    }
   *
   *  staticModule("foo.B") will resolve to the symbol corresponding to the object B declared in the package foo, and
   *  staticModule("foo.A") will throw a MissingRequirementException (which is exactly what scalac would do if this
   *  fully qualified class name is written inside any package in a Scala program).
   *
   *  In the example above, to load a symbol that corresponds to the object B declared in the object foo,
   *  use staticModule("foo") to load the module symbol and then navigate typeSignature.members of its moduleClass.
   */
  def staticModule(fullName: String): U#ModuleSymbol

  /** The symbol corresponding to a package with the
   *  given fully qualified name `fullName`.
   */
  def staticPackage(fullName: String): U#ModuleSymbol
}
