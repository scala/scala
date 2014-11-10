abstract class NoAbstract

abstract class TwoAbstract { def ap(a: Int): Int; def pa(a: Int): Int }

abstract class Base // check that the super class constructor isn't considered.
abstract class NoEmptyConstructor(a: Int) extends Base { def this(a: String) = this(0); def ap(a: Int): Int }

abstract class OneEmptyConstructor() { def this(a: Int) = this(); def ap(a: Int): Int }

abstract class OneEmptySecondaryConstructor(a: Int) { def this() = this(0); def ap(a: Int): Int }

abstract class MultipleConstructorLists()() { def ap(a: Int): Int }

abstract class MultipleMethodLists()() { def ap(a: Int)(): Int }

abstract class ImplicitConstructorParam()(implicit a: String) { def ap(a: Int): Int }

abstract class ImplicitMethodParam() { def ap(a: Int)(implicit b: String): Int }

abstract class PolyClass[T] { def ap(a: T): T }

abstract class PolyMethod { def ap[T](a: T): T }

abstract class OneAbstract { def ap(a: Int): Any }
abstract class DerivedOneAbstract extends OneAbstract

object Test {
  implicit val s: String = ""
  type NonClassType = DerivedOneAbstract with OneAbstract

  (() => 0)      : NoAbstract
  ((x: Int) => 0): TwoAbstract
  ((x: Int) => 0): DerivedOneAbstract           // okay
  ((x: Int) => 0): NonClassType                 // "class type required". I think we should avoid SAM translation here.
  ((x: Int) => 0): NoEmptyConstructor
  ((x: Int) => 0): OneEmptyConstructor          // okay
  ((x: Int) => 0): OneEmptySecondaryConstructor // derived class must have an empty *primary* to call.
  ((x: Int) => 0): MultipleConstructorLists
  ((x: Int) => 0): MultipleMethodLists
  ((x: Int) => 0): ImplicitConstructorParam
  ((x: Int) => 0): ImplicitMethodParam

  ((x: Int) => 0): PolyClass[Int]               // okay
  ((x: Int) => 0): PolyMethod
}
