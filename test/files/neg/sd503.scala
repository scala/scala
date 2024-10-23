//> using options -Wmultiarg-infix
//
// lint multiarg infix syntax, e.g., vs += (1, 2, 3)
// Since infix is encouraged by symbolic operator names, discourage defining def += (x: A, y: A, zs: A*)

trait T {
  def m(i: Int, j: Int) = i + j

  def % (i: Int, j: Int) = i + j      // operator, warn


  def f0(t: T) = t.m(1, 2)            // ok
  def f1(t: T) = t m (1, 2)           // multiarg, warn
  def f2(t: T) = t.%(1, 2)            // ok
  def f3(t: T) = t % (1, 2)           // multiarg, warn

  def c = new C
  def f4() = c.x = (42, 27)           // missing arg
  def f5() = c x_= (42, 27)           // multiarg, warn

  def d = new D
  def f6() = d.x = 42                 // ok!
  def f7() = d.x = (42, 27)           // type mismatch (same as doti)
}

class C {
  private var value: Int = _
  def x: Int = value
  def x_=(i: Int, j: Int): Unit = value = i + j           // multiarg, but don't warn
}
class D {
  private var devalue: Int = _                            // d.value
  def x: Int = devalue
  def x_=(i: Int, j: Int = 1): Unit = devalue = i + j     // multiarg, but don't warn
}

// If the application is adapted such that what looks like a tuple is taken as a tuple,
// then don't warn; eventually this will be normal behavior.
trait OK {
  def f(p: (Int, Int)) = p == (42, 17)                    // nowarn!
  def g(ps: Embiggen[(Int, Int)]) = ps :+ (42, 17)        // nowarn!

  def ?[A: List: reflect.ClassTag] = ???                  // nowarn!

  def calculate(): Int = {
    def ++(i: Int, j: Int): Int = i + j                   // nowarn!
    ++ (42, 17)
  }
}

// Growable is the paradigmatic example
trait Embiggen[A] {
  def addAll(as: Seq[A]): this.type
  def +=(x: A, y: A, zs: A*): this.type = addAll(x +: y +: zs)                // very multiarg, warn
  def :+(a: A): Embiggen[A]
}

trait NotOK {
  def f[A](as: Embiggen[A], x: A, y: A, z: A): as.type = as += (x, y, z)      // very multiarg, warn
}

// Don't warn if deprecated or not used
trait Exceptions[A] {
  def addAll(as: Seq[A]): this.type
  @deprecated("Quit using old infix style!", since="lately")
  def +=(x: A, y: A, zs: A*): this.type = addAll(x +: y +: zs)                // nowarn!
  @annotation.unused
  private def ++=(x: A, y: A, zs: A*): this.type = addAll(x +: y +: zs)       // nowarn!

  def f(x: A, y: A, zs: A*): this.type = this += (x, y, zs: _*)               // warn but could defer to deprecation
}
@deprecated("Quit using old infix style!", since="lately")
trait AlsoExceptions[A] {
  def addAll(as: Seq[A]): this.type
  def +=(x: A, y: A, zs: A*): this.type = addAll(x +: y +: zs)                // nowarn!
}

trait WhyNamingIsHard {
  def lines_!(x: Int, y: Int): List[String] = ???                             // nowarn, give it a pass
  def f = this lines_! (42, 27)                                               // warn usage, of course
}

class A(a: Int, b: Int)(c: Int = 1) // nowarn on <synthetic> def <init>$default$3(a: Int, b: Int): Int = 1

case class Adder(c: Int) {
  def +(i: Int, j: Int): Adder = new Adder(c + i*j)   // warn multiarg symbolic def
}

object Test extends App {
  println {
    var x = new Adder(42)
    x = x + (3, 9)                 // warn multiarg infix apply
    x
  }
  println {
    var x = new Adder(42)
    x.+=(3, 9)                     // nowarn
    x
  }
  println {
    var x = new Adder(42)
    x += (3, 9)                    // warn multiarg infix assignment!
    x
  }
}
