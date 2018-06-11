trait Provider[T] {
  def provide: T
}

class Provides[T] {
  def provide(t: T): Provider[T] = new Provider[T] { def provide = t }
}

object Test extends App {

  val ctor = Class.forName("Provides$$anon$1")
    .getDeclaredConstructors
    .head

  println(ctor.getParameters.map(_.getParameterizedType).toList)
  println(ctor.getGenericParameterTypes.toList)

}