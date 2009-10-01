object Test  {
	class A

	class B[X]
	type Alias[X <: A] = B[X]

	class C extends Alias[Any]   // not ok, normalisation should check bounds before expanding Alias
}