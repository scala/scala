object Test extends App {
  def foo = {

    abstract class MouseEventType { def x: String }
    case object Clicked extends MouseEventType {
      def x = "Clicked"
    }

    trait MouseHandler {
      def mouseClicked() = handleEvent(Clicked);
      def handleEvent(t : MouseEventType) = t.x
    }
    (new MouseHandler {}).handleEvent(Clicked)
  }
  assert(foo == "Clicked")
}

