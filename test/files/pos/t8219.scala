trait Equalizer[T]
trait Gen[A]
 
class Broken {
  implicit def const[T](x: T): Gen[T] = ???
  implicit def convertToEqualizer[T](left: T): Equalizer[T] = ???
 
  def in(a: Any) = ()
  in {
    import scala.None // any import will do..
    "" == ""
  }

  // We fall into the errant code path above because `Any#==` and `AnyRef#==`
  // are (currently) overloaded.
  //
  // Real classes couldn't get away with that overloading; it would result in
  // a compiler error because the variants would collapse into an overriding
  // relationship after erasure.
  //
  // 
  // But, a structural type can! This triggers the same error, and served as
  // a backstop for this test if we change the signatures of `AnyRef#==` to
  // override `Any#==`.
  type T = { 
    def a(a: AnyRef): Boolean
    def a(a: Any): Boolean
  }

  def t: T = ???

  in {
    import scala.None // any import will do..
    t.a("")
  }
}
