abstract class Component

class Button extends Component {
  def sayHey: Unit = Console.println("Hey, I'm a button") }

abstract class Origin {
  val delegate: Component }

object main extends Origin with App {
  val delegate: Component {
    def sayHey: Unit
  } = new Button
  delegate.sayHey
}
