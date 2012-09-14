import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => m}
import scala.reflect.runtime.{universe => u}

object Test extends App {
  locally {
    try {
      case class Test(a:String,b:List[Int])

      val lookAtMe = m.reflect(Test("a",List(5)))
      val value = u.typeOf[Test]
      val members = value.members
      val member = value.members.filter(_.name.encoded == "a")
      val aAccessor = lookAtMe.reflectMethod(member.head.asMethod)
      val thisShouldBeA = aAccessor.apply()
      println(thisShouldBeA)
    } catch {
      case ScalaReflectionException(msg) => println(msg)
    }
  }
}