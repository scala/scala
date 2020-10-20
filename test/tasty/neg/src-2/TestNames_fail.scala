package tastytest

import Names._

object TestNames {

  val Some(tom) = Name.read("Tom")

  def initialOfName = initial(tom) // ok, (tom: Name)
  def initialOfString = initial("Tom") // error, ("Tom": String)

}
