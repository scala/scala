class A {
    def whatever() {
        lazy val a = 1
        lazy val b = try { 2 } catch { case _ => 0 }
        a
        b

    }
}

object Test {
    def main(a: Array[String]) {
        val a = new A
        a.whatever
    }
}