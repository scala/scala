class Parent {
  def twice[@scala.specialized(Int) I <: Int : ValueOf]: Int = valueOf[I] * 2
}

object Test extends App {
  val actualMethods = (new Parent).getClass.getDeclaredMethods
  assert(actualMethods.count(_.getName == "twice$mIc$sp") == 1)
}