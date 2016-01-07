class Bippo {
  def length: Int = 123
  class Tree
}

package object p1 {
  class A
  implicit class B(val s: String) { def bippy = s }
  val c: Bippo = new Bippo
  type D = String
}
package object p2 {
  class A
  implicit class B(val s: String) { def bippy = s }
  val c: Bippo = new Bippo
  type D = Int
}

trait NoWarn {
  {
    import p1._ // no warn
    println("abc".bippy)
  }

  {
    import p1._ // no warn
    println(new A)
  }

  {
    import p1.B // no warn
    println("abc".bippy)
  }

  {
    import p1._ // no warn
    import c._  // no warn
    println(length)
  }

  {
    import p1._ // no warn
    import c._  // no warn
    val x: Tree = null
    println(x)
  }

  {
    import p1.D // no warn
    val x: D = null
    println(x)
  }
}

trait Warn {
  {
    import p1.A // warn
    println(123)
  }

  {
    import p1.{ A, B } // warn on A
    println("abc".bippy)
  }

  {
    import p1.{ A, B } // warn on both
    println(123)
  }

  {
    import p1._ // no warn (technically this could warn, but not worth the effort to unroll unusedness transitively)
    import c._  // warn
    println(123)
  }

  {
    import p1._ // warn
    println(123)
  }

  {
    class Tree
    import p1._ // no warn
    import c._  // warn
    val x: Tree = null
    println(x)
  }

  {
    import p1.c._  // warn
    println(123)
  }
}

trait Nested {
  {
    import p1._   // warn
    trait Warn {  // warn about unused local trait for good measure
      import p2._
      println(new A)
      println("abc".bippy)
    }
    println("")
  }

  {
    import p1._   // no warn
    trait NoWarn {
      import p2.B  // no warn
      println("abc".bippy)
      println(new A)
    }
    println(new NoWarn { })
  }

  {
    import p1.A   // warn
    trait Warn {
      import p2.A
      println(new A)
    }
    println(new Warn { })
  }
}

// test unusage of imports from other compilation units after implicit search
trait Outsiders {
  {
    //implicit search should not disable warning
    import Sample._
    import Sample.Implicits._   // warn
    f(42)                       // error
  }
  {
    import Sample._
    import Sample.Implicits._   // nowarn
    g(42)                       // ok
  }
  {
    import Sample._
    import Sample.Implicits.`int to Y`  // nowarn
    import Sample.Implicits.useless     // warn
    g(42)                       // ok
  }
  {
    import java.io.File                 // warn
    import scala.concurrent.Future      // warn
    import scala.concurrent.ExecutionContext.Implicits.global // warn
    import p1.A                         // warn
    import p1.B                         // no warn
    println("abc".bippy)
    //Future("abc".bippy)
  }
}
