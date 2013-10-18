class NoAbstract

class TwoAbstract { def ap(a: Int): Int; def pa(a: Int): Int }

class Base // check that the super class constructor isn't considered.
class NoEmptyConstructor(a: Int) extends Base { def this(a: String) = this(0); def ap(a: Int): Int }

class OneEmptyConstructor() { def this(a: Int) = this(); def ap(a: Int): Int }

class OneEmptySecondaryConstructor(a: Int) { def this() = this(0); def ap(a: Int): Int }

class MultipleConstructorLists()() { def ap(a: Int): Int }

class MultipleMethodLists()() { def ap(a: Int)(): Int }

class ImplicitConstructorParam()(implicit a: String) { def ap(a: Int): Int }

class ImplicitMethodParam() { def ap(a: Int)(implicit b: String): Int }

class PolyClass[T] { def ap(a: T): T }

class PolyMethod { def ap[T](a: T): T }

class OneAbstract { def ap(a: Any): Any }
class DerivedOneAbstract extends OneAbstract

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
