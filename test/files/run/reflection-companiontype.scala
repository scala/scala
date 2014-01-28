import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}

class C
object C

object Test extends App {
  type T = C

  println("TypeRefs")
  println(showRaw(typeOf[C].companionType, printKinds = true))
  println(showRaw(typeOf[C].companionType.companionType, printKinds = true))
  println(showRaw(typeOf[C.type].companionType, printKinds = true))
  println("ClassInfoTypes")
  println(showRaw(typeOf[C].typeSymbol.typeSignature.companionType, printKinds = true))
  println(showRaw(typeOf[C].typeSymbol.typeSignature.companionType.typeSymbol.typeSignature.companionType, printKinds = true))
  println(showRaw(typeOf[C.type].typeSymbol.typeSignature.companionType, printKinds = true))
  println("Unrelated")
  println(showRaw(typeOf[T].companionType, printKinds = true))
  println(showRaw(cm.staticPackage("scala").moduleClass.asType.toType.companionType, printKinds = true))
  println(showRaw(cm.staticPackage("scala").typeSignature.companionType, printKinds = true))
}