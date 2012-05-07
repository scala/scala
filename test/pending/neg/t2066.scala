object Test extends App {
	trait A {
	 def f[T[_]](x : T[Int]) : T[Any]
	}

	class B extends A {
	 def f[T[+_]](x : T[Int]) : T[Any] = x
	}

	class P[Y](var y : Y)

	val p = new P(1)
	val palias = (new B():A).f[P](p)
	palias.y = "hello"	
	val z: Int = p.y
}