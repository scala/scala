//
// Tests that the static accessor method for lambda bodies
// (generated under -Ydelambdafy:method) are compatible with
// Java 8's LambdaMetafactory.
//
import java.lang.invoke._

class C {
  def test1: Unit = {
    (x: String) => x.reverse
  }
  def test2: Unit = {
    val capture1 = "capture1"
    (x: String) => capture1 + " " + x.reverse
  }
  def test3: Unit = {
    (x: String) => C.this + " " + x.reverse
  }
}
trait T {
  def test4: Unit = {
    (x: String) => x.reverse
  }
}

// A functional interface. Function1 contains abstract methods that are filled in by mixin 
trait Function1ish[A, B] {
  def apply(a: A): B
}

object Test {
  def lambdaFactory[A, B](hostClass: Class[_], instantiatedParam: Class[A], instantiatedRet: Class[B], accessorName: String,
                          capturedParams: Array[(Class[_], AnyRef)] = Array()) = {
    val caller = MethodHandles.lookup
    val methodType = MethodType.methodType(classOf[AnyRef], Array[Class[_]](classOf[AnyRef]))
    val instantiatedMethodType = MethodType.methodType(instantiatedRet, Array[Class[_]](instantiatedParam))
    val (capturedParamTypes, captured) = capturedParams.unzip
    val targetMethodType = MethodType.methodType(instantiatedRet, capturedParamTypes :+ instantiatedParam)
    val invokedType = MethodType.methodType(classOf[Function1ish[_, _]], capturedParamTypes)
    val target = caller.findStatic(hostClass, accessorName, targetMethodType)
    val site = LambdaMetafactory.metafactory(caller, "apply", invokedType, methodType, target, instantiatedMethodType)
    site.getTarget.invokeWithArguments(captured: _*).asInstanceOf[Function1ish[A, B]]
  }
  def main(args: Array[String]) {
    println(lambdaFactory(classOf[C], classOf[String], classOf[String], "accessor$1").apply("abc"))
    println(lambdaFactory(classOf[C], classOf[String], classOf[String], "accessor$2", Array(classOf[String] -> "capture1")).apply("abc"))
    println(lambdaFactory(classOf[C], classOf[String], classOf[String], "accessor$3", Array(classOf[C] -> new C)).apply("abc"))
    println(lambdaFactory(Class.forName("T$class"), classOf[String], classOf[String], "accessor$4", Array(classOf[T] -> new T{})).apply("abc"))
  }
}
