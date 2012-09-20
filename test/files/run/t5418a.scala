object Test extends App {
  println(scala.reflect.runtime.universe.reify(new Object().getClass))
}