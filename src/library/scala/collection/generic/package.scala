package scala.collection
import generic.CanBuildFrom

package object generic {
  type CanBuild[-Elem, +To] = CanBuildFrom[Nothing, Elem, To]
}