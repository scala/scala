class A {
    def whatever(): Unit = {
        lazy val a = 1
        lazy val b = try { 2 } catch { case _: Throwable => 0 }
        a
        b

    }
}

object Test {
    def main(a: Array[String]): Unit = {
        val a = new A
        a.whatever
    }
}
