import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  val code = scala.reflect.Code.lift{
    /** A mutable property whose getter and setter may be customized. */
    case class Property[T](init: T) {
      private var value: T = init

      /** The getter function, defaults to identity. */
      private var setter: T => T = identity[T]

      /** The setter function, defaults to identity. */
      private var getter: T => T = identity[T]

      /** Retrive the value held in this property.   */
      def apply(): T = getter(value)

      /** Update the value held in this property, through the setter. */
      def update(newValue: T) = value = setter(newValue)

      /** Change the getter. */
      def get(newGetter: T => T) = { getter = newGetter; this }

      /** Change the setter */
      def set(newSetter: T => T) = { setter = newSetter; this }
    }

    class User {
      // Create a property with custom getter and setter
      val firstname = Property("")
        .get { v => v.toUpperCase() }
        .set { v => "Mr. " + v }
      val lastname = Property("<noname>")

      /** Scala provides syntactic sugar for calling 'apply'. Simply
       *  adding a list of arguments between parenthesis (in this case,
       *  an empty list) is translated to a call to 'apply' with those
       *  arguments.
       */
      override def toString() = firstname() + " " + lastname()
    }

    val user1 = new User

    // Syntactic sugar for 'update': an assignment is translated to a
    // call to method 'update'
    user1.firstname() = "Robert"

    val user2 = new User
    user2.firstname() = "bob"
    user2.lastname() = "KUZ"

    println("user1: " + user1)
    println("user2: " + user2)
  };

  val reporter = new ConsoleReporter(new Settings)
  val toolbox = new ToolBox(reporter)
  val ttree = toolbox.typeCheck(code.tree)
  toolbox.runExpr(ttree)
}
