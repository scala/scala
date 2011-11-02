package case1 {
  class Bippy {
    def f = 1
  }

  trait Foo {
    object Bar extends Bippy {
      override def f = 2
    }
  }

  trait Foo2 extends Foo {
    override object Bar extends Bippy {
      override def f = 3
    }
  }
  
  trait Foo3 {      
    object Bar {
      def g: Traversable[Int] = Nil
    }
  }
  trait Foo4 extends Foo3 {
    override object Bar {
      def g: List[Int] = Nil
    }
  }
}

package case2 {
  class Bar[T]

  class Foo[T] {
    object A extends Bar[T]
  }

  class Baz[S] extends Foo[S] {
    override object A extends Bar[S]
  }
}
