import scala.collection.mutable.HashMap

object AssertionError extends AnyRef with App
{
    abstract class A {}

    object A1 extends A {}

    object A2 extends A {}

    class Manager
    {
        final class B {}
    
        val map = new HashMap[A, B]
    }
    
        
    def test[T](f: => T) { f }

    test {
        val manager = new Manager

        // This line is illegal and causes a compiler crash with Scala 2.3.1
        assert(manager.map(A2) == List(manager.map(A2, A1)))
    }

}
