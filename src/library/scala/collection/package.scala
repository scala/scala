package scala

package object collection {
  import scala.collection.generic.CanBuildFrom // can't refer to CanBuild here

  /** Provides a CanBuildFrom instance that builds a specific target collection (`To') irrespective of the original collection (`From').
   */
  def breakOut[From, T, To](implicit b : CanBuildFrom[Nothing, T, To]) =
    new CanBuildFrom[From, T, To] { // TODO: could we just return b instead?
      def apply(from: From) = b.apply() ; def apply() = b.apply()
    }
}