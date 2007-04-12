abstract class  A {
  override def toString(): String // crucial

  def toString(sb: StringBuilder): StringBuilder // crucial
}

case class B extends A {
  // overloaded version is implemented, causing toString not to be implemented?
  def toString(sb: StringBuilder): StringBuilder = error("")
}

object Test extends Application {
  Console.println(B)
}