import language.higherKinds

trait T {
  def t = 0
}
trait Foo {
  def coflatMap[A <: T](f: A): A
}

object O extends Foo {
  def coflatMap[A <: T](f: A) = {
    val f2 = coflatMap(f) // inferred in 2.9.2 / 2.10.0 as [Nothing]
    f2.t                  // so this fails to type check.
    f2
  }
}

// Why? When a return type is inherited, the derived method
// symbol first gets a preliminary type assigned, based on the
//   1) method type of a unique matching super member
//   2) viewed as a member type of the inheritor (to substitute,
//      e.g. class type parameters)
//   3) substituted to replace the super-method's type parameters
//      with those of the inheritor
//   4) dissected to take just the return type wrapped in thisMethodType().
//
// In Scala 2.10.0 and earlier, this preliminary method type
//
//   1) [A#11329 <: <empty>#3.this.T#7068](<param> f#11333: A#11329)A#11329
//   2) [A#11329 <: <empty>#3.this.T#7068](<param> f#11333: A#11329)A#11329
//   3) (<param> f#12556: A#11336)A#11336
//   4) [A#11336 <: <empty>#3.this.T#7068](<param> f#12552: A#11337&0)A#11336
//
// The type #4 from the old version is problematic: the parameter is typed with
// a skolem for the type parameter `A`. It won't be considered to match the
// method it overrides, instead they are seen as being overloaded, and type inference
// goes awry (Nothing is inferred as the type argument for the recursive call
// to coflatMap.
//
// The Namers patch adds one step here: it subsitutes the type parameter symbols
// for the skolems:
//
//  https://github.com/scala/scala/commit/b74c33eb#L2R1014
//
// So we end up with a method symbol info:
//
//   5) [A#11336 <: <empty>#3.this.T#7068](<param> f#12505: A#11336)A#11336
//
// This *does* match the method in the super class, and type inference
// chooses the correct type argument.