trait A

object Test {
    lazy val a = new A {
        object Zenek
    }
}
