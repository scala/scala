class Lift {
  def foo = {
    // this will be captured by the MouseHandler trait,
    // which gives rise to a new trait field during LambdaLift
    var Clicked = "Clicked"

    def bar = Clicked

    trait MouseHandler {
      def mouseClicked = Clicked + bar
    }

    class CC extends MouseHandler

    // new C {}
    (new CC).mouseClicked
  }
}

object O extends Lift with App {
  println(foo)
}
