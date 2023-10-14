import tastytest.ExperimentalObj
import tastytest.ExperimentalClass
import scala.annotation.compileTimeOnly
import ExperimentalDefsPre._

object ExperimentalDefs {

  class Box[T]

  def termRef = ExperimentalObj.foo // error
  def typeRef = new Box[ExperimentalClass]() // error

  class SubExperimental extends ExperimentalClass // error
  class SuperExperimental extends Box[ExperimentalClass] // error
  type RefExperimental = Box[ExperimentalClass] // error
  type MemberExperimental = { type Ref = ExperimentalClass } // error
  type AnnotatedRef = Box[Int] @ExperimentalClass // error

  def refSubclassOfExperimental = new SubExperimentalNotExperimental() // error

  @compileTimeOnly("")
  class OnlyAtCompileTime

  def fixMe1 = List[ExperimentalClass]() // TODO: why is checkUndesiredProperties not called?
  def fixMe2 = List.apply[ExperimentalClass]() // TODO: why is checkUndesiredProperties not called?
  def fixMe3 = List[OnlyAtCompileTime]() // TODO: why is checkUndesiredProperties not called?
  def fixMe4 = List.apply[OnlyAtCompileTime]() // TODO: why is checkUndesiredProperties not called?
}
