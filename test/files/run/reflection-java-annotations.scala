object Test extends App {
  import scala.reflect.runtime.universe._
  val sym = typeOf[JavaAnnottee].typeSymbol
  sym.typeSignature
  sym.getAnnotations foreach (_.javaArgs)
  println(sym.getAnnotations)
}