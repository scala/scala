import java.io.File
import java.net.URLClassLoader

object StepTwo extends App {
  import scala.reflect.runtime.universe._
  println(typeOf[StepTwo.type])
}