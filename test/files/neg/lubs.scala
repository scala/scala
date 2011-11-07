object test1 {
  abstract class A[+T]
  class C extends A[C]
  class D extends A[D]

  def f = if(1 == 2) new C else new D

  val x1: A[Any] = f
  val x2: A[A[Any]] = f
  val x3: A[A[A[Any]]] = f
  val x4: A[A[A[A[Any]]]] = f
}

object test2 {

  abstract class A { type T }
  class C extends A { type T = C }
  class D extends A { type T = D }

  def f = if (1 == 2) new C else new D
    
  val x1: A { type T } = f
  val x2: A { type T >: Null <: A } = f
  val x3: A { type T >: Null <: A { type T >: Null <: A } } = f
  val x4: A { type T >: Null <: A { type T >: Null <: A { type T >: Null <: A } } } = f
}
