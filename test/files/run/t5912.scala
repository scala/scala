import scala.language.existentials
object Test extends App{
  import scala.reflect.runtime.{currentMirror=>cm}
  import scala.tools.reflect._
  import scala.reflect.runtime.universe._
  val tree = cm.mkToolBox().typecheck( Literal(Constant("test")) )
}
