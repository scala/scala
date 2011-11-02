trait Syntax {
  object Foo
}

trait Evaluation {
  val syntax: Syntax
  
  def equalInTrait = this.syntax.Foo == this.syntax.Foo
}

object Test extends Evaluation with App {
  object syntax extends Syntax 

  def equalInObject = this.syntax.Foo == this.syntax.Foo
  
  println(equalInTrait)
  println(equalInObject)
}
