// test no longer applicable. but I think the underlying problem is fixed in trunk

scala> implicit def toJavaList[A](t:collection.Sequence[A]):java.util.List[A] =
     | java.util.Arrays.asList(t.toArray:_*)   java.util.Arrays.asList(t.toArray:_*)
toJavaList: [A](t: Sequence[A])java.util.List[A]

scala> val x: java.util.List[String] = List("foo")
<console>:7: error: type mismatch;
 found   : List[Any]
 required: java.util.List[String]
       val x: java.util.List[String] = List("foo")
