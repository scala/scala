abstract class A {
    
    lazy val lazyBar = bar
    
    object bar {        
        val foo = 12
    }

}

object Test extends Application {
    val a = new A{}
    println(a.lazyBar.foo)
}
