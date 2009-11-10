package scala

package object collection {
  import scala.collection.generic.CanBuildFrom // can't refer to CanBuild here

  def breakOut[From, T, To](implicit b : CanBuildFrom[Nothing, T, To]) =
    new CanBuildFrom[From, T, To] {
      def apply(from: From) = b.apply() ; def apply() = b.apply()
    }
}