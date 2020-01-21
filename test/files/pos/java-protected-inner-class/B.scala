import a._, A._
object W extends A {
  new I // error: Unable to emit reference to constructor I in class I, class I is not accessible in object W
  new S // error: Unable to emit reference to constructor S in class S, class S is not accessible in object W
}
