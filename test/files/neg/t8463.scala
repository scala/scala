object Test {
  case class Foo[+T[_]](activity:T[Long])
  type Cell[T] = T
  def insertCell(u:Foo[Cell]) = ???
  insertCell(Foo(5))
}

/* If SI-8230 is fixed, and `viewExists` is changed to no longer leak
   ambiguity errors, you might expect the check file for this test to
   change as follows:

@@ -1,18 +1,10 @@
-t8463.scala:5: error: no type parameters for method apply: (activity:
- --- because ---
-argument expression's type is not compatible with formal parameter ty
+t8463.scala:5: error: type mismatch;
  found   : Long
  required: ?T[Long]
+Note that implicit conversions are not applicable because they are am
+ both method longWrapper in class LowPriorityImplicits of type (x: Lo
+ and method ArrowAssoc in object Predef of type [A](self: A)ArrowAsso
+ are possible conversion functions from Long to ?T[Long]
   insertCell(Foo(5))
-             ^
-t8463.scala:5: error: type mismatch;
- found   : Long(5L)
- required: T[Long]
-  insertCell(Foo(5))
-                 ^
-t8463.scala:5: error: type mismatch;
- found   : Test.Foo[T]
- required: Test.Foo[Test.Cell]
-  insertCell(Foo(5))
-                ^
-three errors found
+            ^
+one error found
*/
