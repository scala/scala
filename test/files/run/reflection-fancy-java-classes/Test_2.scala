import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.partest._

object Test extends App {
  val fancyName = ifJavaAtLeast("9")("Foo_1$Bar") otherwise "Foo_1$1"
  val jfancy = Class.forName(fancyName)
  println("===== JAVA POV =====")
  println(jfancy)
  println("getEnclosingClass = " + jfancy.getEnclosingClass)
  println("getEnclosingMethod = " + jfancy.getEnclosingMethod)
  println("getEnclosingConstructor = " + jfancy.getEnclosingConstructor)
  println("isMemberClass = " + jfancy.isMemberClass)
  println("isLocalClass = " + jfancy.isLocalClass)
  println("isAnonymousClass = " + jfancy.isAnonymousClass)

  println("")
  println("===== SCALA POV =====")
  val sfancy = cm.classSymbol(jfancy)
  println(sfancy)
  println(sfancy.owner)
}