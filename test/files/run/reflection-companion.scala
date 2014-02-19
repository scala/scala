import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}

class C
object C

object Test extends App {
  type T = C

  println(showRaw(symbolOf[C].companion, printKinds = true))
  println(showRaw(symbolOf[C].companion.companion, printKinds = true))
  println(showRaw(symbolOf[C.type].companion, printKinds = true))
  println(showRaw(symbolOf[T].companion, printKinds = true))
  println(showRaw(cm.staticPackage("scala").moduleClass.companion, printKinds = true))
  println(showRaw(cm.staticPackage("scala").companion, printKinds = true))
}