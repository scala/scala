trait A[@specialized(Int) T] { def foo: T }
class B extends A[Int] { val foo = 10 }
class C extends B

// issue 3309
class Lazy {
  def test[U](block: => U): Unit = { block }

  test { lazy val x = 1 }
}

// issue 3307
class Bug3307 {
  def f[Z](block: String => Z) { 
    block("abc") 
  }
  
  ({ () => 
    f { implicit x => println(x) } })() 
}

// issue 3301
  trait T[X]

class Bug3301 {
  def t[A]: T[A] = error("stub")

  () => {
    type X = Int

    def foo[X] = t[X]
    ()
  }
}
// issue 3299
object Failure {
  def thunk() {
    for (i <- 1 to 2) {
      val Array(a, b) = Array(1,2)
      ()
    }
  }
}

// issue 3296

object AA
{
    def f(block: => Unit) {}

    object BB
    {
        f {
            object CC

            ()
        }
    }

  def foo[T](x: T) = { object A; false }
}

// issue 3325
object O { def f[@specialized T] { for(k <- Nil: List[T]) { } } }
