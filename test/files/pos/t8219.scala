trait Equalizer[T]
trait Gen[A]

class Broken {
  implicit def const[T](x: T): Gen[T] = ???
  implicit def convertToEqualizer[T](left: T): Equalizer[T] = ???

  def in(a: Any) = ()
  in {
    import scala.None // any import will do..
    "" == "" // this no longer triggers the bug, as Object#== now overrides Any#==
  }

  // We can still trigger the bug with a structural type, see pending/neg/t8219.scala
}
