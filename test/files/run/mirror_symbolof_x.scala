import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.reflect.api.Mirror

class C
object C

object Test extends App {
  object test1 {
    val m = cm
    type T = Int
    type Id[X] = X
    println(m.symbolOf[Int]: ru.TypeSymbol)
    println(m.symbolOf[C.type]: ru.TypeSymbol)
    println(m.symbolOf[T]: ru.TypeSymbol)
    println(m.symbolOf[Id[_]]: ru.TypeSymbol)
    println(m.symbolOf[Nothing]: ru.TypeSymbol)
    println(m.symbolOf[Null]: ru.TypeSymbol)
  }

  object test2 {
    val m: Mirror[ru.type] = cm
    type T = Int
    type Id[X] = X
    println(m.symbolOf[Int]: ru.TypeSymbol)
    println(m.symbolOf[C.type]: ru.TypeSymbol)
    println(m.symbolOf[T]: ru.TypeSymbol)
    println(m.symbolOf[Id[_]]: ru.TypeSymbol)
    println(m.symbolOf[Nothing]: ru.TypeSymbol)
    println(m.symbolOf[Null]: ru.TypeSymbol)
  }

  object test3 {
    val m = ru.runtimeMirror(classOf[Int].getClass.getClassLoader)
    try println(m.symbolOf[C])
    catch { case ex: ScalaReflectionException => println(s"exception: ${ex.getMessage}") }
  }

  test1
  test2
  test3
}
