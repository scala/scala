object Test  {
	class A

	class B[X]
	type Alias[X <: A] = B[X]

	val foo: B[A] = new Alias[A] // check that type aliases can be instantiated
}