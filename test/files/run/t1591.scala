abstract class A {

    lazy val lazyBar = bar

    object bar {
        val foo = 12
    }

}

object Test extends App {
    val a = new A{}
    println(a.lazyBar.foo)
}
