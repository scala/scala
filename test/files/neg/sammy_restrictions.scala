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

abstract class SelfTp { self: NoAbstract => def ap(a: Int): Any }
abstract class SelfVar { self => def ap(a: Int): Any }

trait T1 { def t(a: Int): Int }; trait U1

object Test {
  implicit val s: String = ""
  type NonClassTypeRefinement = DerivedOneAbstract with OneAbstract
  type NonClassType = DerivedOneAbstract

  // errors:
  def f0 = (() => 0)      : NoAbstract
  def f1 = ((x: Int) => 0): TwoAbstract
  def f2 = ((x: Int) => 0): NoEmptyConstructor
  def f3 = ((x: Int) => 0): MultipleConstructorLists
  def f4 = ((x: Int) => 0): OneEmptySecondaryConstructor // derived class must have an empty *primary* to call.
  def f5 = ((x: Int) => 0): MultipleMethodLists
  def f6 = ((x: Int) => 0): ImplicitConstructorParam
  def f7 = ((x: Int) => 0): ImplicitMethodParam
  def f8 = ((x: Int) => 0): PolyMethod
  def f9 = ((x: Int) => 0): SelfTp
  def g0 = ((x: Int) => 0): T1 with U1
  def g1 = ((x: Int) => 0): NonClassTypeRefinement

  // allowed:
  def g2 = ((x: Int) => 0): OneEmptyConstructor
  def g3 = ((x: Int) => 0): DerivedOneAbstract
  def g4 = ((x: Int) => 0): NonClassType                 // we also allow type aliases in instantiation expressions, if they resolve to a class type
  def g5 = ((x: Int) => 0): PolyClass[Int]
  def g6 = ((x: Int) => 0): SelfVar
}
