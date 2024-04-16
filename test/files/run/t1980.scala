//> using options -Yrangepos
//
class LazyList[+A](expr: => LazyList.Evaluated[A]) {
  def #:: [B >: A](elem: => B): LazyList[B] = new LazyList(Some((elem, this)))
  def ##:: [B >: A](elem: B): LazyList[B] = new LazyList(Some((elem, this)))
  def force: Unit = expr.foreach(_._2.force)
}

object LazyList {
  type Evaluated[+A] = Option[(A, LazyList[A])]
  object Empty extends LazyList[Nothing](None)
  def empty[A]: LazyList[A] = Empty
}

object Test extends App {
  def foo(i: Int) = { println("foo "+i); i }
  def bar(i: Int) = { println("bar "+i); i }
  println("1. defining")
  val xs1 = foo(1) ##:: foo(2) ##:: foo(3) ##:: LazyList.empty
  val xs2 = bar(1) #:: bar(2) #:: bar(3) #:: LazyList.empty
  println("1. forcing")
  xs1.force
  xs2.force

  {
    println("2. defining")
    class C { def f_:(x: => Int)(implicit y: Int): () => Int = (() => x) }
    val c = new C
    implicit val i = 1
    def k = { println("hi"); 1 }
    val x1 = c.f_:(k)
    val x2 = k f_: c
    println("2. forcing")
    println(x1())
    println(x2())
  }

  {
    println("3. defining")
    class C { def f_:[T](x: => T): () => T = (() => x) }
    val c = new C
    def k = { println("hi"); 1 }
    val x1 = k f_:[Any] c
    println("3. forcing")
    println(x1())
  }

  // Ensure the owner chain is changed correctly when inlining
  {
    println("4. defining")
    class C { def f_:(x: => Int): () => Int = (() => x) }
    val c = new C
    val x1 = c.f_:({ val xxx = 1; xxx })
    val x2 = { val yyy = 1; yyy } f_: c
    val x3 = { val yyy = 1; yyy } f_: c
    println("4. forcing")
    println(x1())
    println(x2())
    println(x3())
  }

  // Overloaded operator with by-name and by-value variants
  {
    println("5. defining")
    class C {
      val saved = new collection.mutable.ArrayBuffer[() => String]
      def force: Unit = saved.foreach(_.apply())
      def :: (x: Int): C = this
      def :: (x: => String): C = { saved += (() => x); this }
    }
    def genI(i: Int): Int = { println("Int "+i); i }
    def genS(s: String): String = { println("String "+s); s }
    val c = genI(1) :: genS("2") :: genI(3) :: genS("4") :: (new C)
    println("5. forcing")
    c.force
  }
}
