import scala.reflect.runtime.universe._

object Test extends App {

  Modifiers(Flag.IMPLICIT) match { case Modifiers(flag, _, _) => assert(flag == Flag.IMPLICIT) }
}