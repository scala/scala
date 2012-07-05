
import reflect.dynamic.DynamicProxy
object Test extends App {
  val proxy = new DynamicProxy {
    val proxyTarget = List(1,2,3)
  }

  try {
    proxy.bippy(42)
  } catch {
    case t: Throwable => println(t)
  }
}
