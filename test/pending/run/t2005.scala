Object Bug {
  def main(args: Array[String]) {
    val a = new Array[Array[Int]](2,2)
    test(a)
  }
  def test[A](t: Array[Array[A]]) {
    val tmp = t(0)
    t(1) = tmp
  }
}
java.lang.ArrayStoreException: scala.runtime.BoxedIntArray
	at scala.runtime.BoxedObjectArray.update(BoxedObjectArray.scala:26)
	at Bug$.test(Bug.scala:12)
	at Bug$.main(Bug.scala:7)
	at Bug.main(Bug.scala)
	at sun.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
	at sun.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:39)
	at sun.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:25)
	at java.lang.reflect.Method.invoke(Method.java:585)
	at scala.tools.nsc.ObjectRunner$$anonfun$run$1.apply(ObjectRunner.scala:75)
	at scala.tools.nsc.ObjectRunner$.withContextClassLoader(ObjectRunner.scala:49)
	at scala.tools.nsc.ObjectRunner$.run(ObjectRunner.scala:74)
	at scala.tools.nsc.MainGenericRunner$.main(MainGenericRunner.scala:154)
	at scala.tools.nsc.MainGenericRunner.main(MainGenericRunner.scala)
