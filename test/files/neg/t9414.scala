import annotation._

class C {
  def foo = {
    class Parent {
      @tailrec def bar: Int = {
        println("here we go again")
        bar
      }
    }
    class Child extends Parent {
      override def bar = 42
    }
  }
}

class D {
  def foo = {
    class Parent {
      @tailrec def bar: Int = {
        println("here we go again")
        bar
      }
    }
    class Child extends Parent
    class GrandChild extends Child {
      override def bar = 42
    }
  }
}

object E {
  sealed class Parent {
    @tailrec def bar: Int = {
      println("here we go again")
      bar
    }
  }
  final class Child extends Parent {
    override def bar = 42
  }
}
