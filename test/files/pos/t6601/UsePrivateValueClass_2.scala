object Test {
	// After the first attempt to make separately compiled value
	// classes respect the privacy of constructors, we got:
	//
	//   exception when typing v.a().==(v.a())/class scala.reflect.internal.Trees$Apply
  //   constructor V in class V cannot be accessed in object Test in file test/files/pos/t6601/UsePrivateValueClass_2.scala
  //   scala.reflect.internal.Types$TypeError: constructor V in class V cannot be accessed in object Test
  def foo(v: V) = v.a == v.a
  def bar(v: V) = v == v
}
