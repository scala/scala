class B {}
class A { self: B =>
    def m(): B = {
        this
    }
}

object Exec{
    def main(args: Array[String]): Unit = {
        val a: A = new A; // should not be allowed
        val b: B = a.m();
    }
}

