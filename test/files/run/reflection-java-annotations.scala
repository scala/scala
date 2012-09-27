object Test extends App {
  import scala.reflect.runtime.universe._
  val sym = typeOf[JavaAnnottee].typeSymbol
  sym.typeSignature
  sym.annotations foreach (_.javaArgs)
  println(sym.annotations)
}