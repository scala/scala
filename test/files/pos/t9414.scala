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
      def notBar = 42
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
      def notBar = 42
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
    def notBar = 42
  }
}

class F {
  def foo = {
    class Parent {
      @tailrec def bar: Int = {
        println("here we go again")
        bar
      }
    }
    class K {
      class Child extends Parent {
        def notBar = 42
      }
      class GrandChild extends Child {
        def neitherBar = 42
      }
    }
  }
}

class G {
  sealed class Parent {
    @tailrec def bar: Int = {
      println("here we go again")
      bar
    }
  }
  def foo = {
    class Child extends Parent {
      def notBar = 42
    }
    class GrandChild extends Child {
      def neitherBar = 42
    }
  }
}
