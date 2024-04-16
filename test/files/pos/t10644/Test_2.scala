//> using options -Xfatal-warnings
object Test {
  // Should not result in the spurious warning:
  //   comparing non-null values of types Product with Serializable
  //   and A.type using `==` will always yield false
  assert(C.objs.head == A)
}
