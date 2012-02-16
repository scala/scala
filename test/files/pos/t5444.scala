// /scala/trac/5444/a.scala
// Mon Feb 13 21:01:45 PST 2012

// Traits require identical names to reproduce.
class Test {
  def a() = {
    trait T {
      def x() = 1
    }
    trait U {
      def x1() = 2
    }
    class Bippy extends T with U { def z() = x() + x1() }
    new Bippy
  }
  def b() {
    trait T {
      def y() = 3
      trait T2 {
        def yy() = 10
      }
    }
    trait U {
      def y1() = 4
      trait T3 {
        def yy() = 11
      }
    }
    class Bippy extends T with U { def z() = y() + y1() + (1 to (new T2 { }).yy()).map(_ + 1).sum }
    (new Bippy).z()
  }
  def c() {
    trait T {
      def z() = 5
    }
    trait U {
      def z1() = 6
    }
    (new Test with T with U).z1()
  }
}

