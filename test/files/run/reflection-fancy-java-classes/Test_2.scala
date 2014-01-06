import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}

object Test extends App {
  println("===== JAVA POV =====")
  val jfancy = Class.forName("Foo_1$1")
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