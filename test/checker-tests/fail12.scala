class A {
  def f(b: Boolean) = {
    locally {
      while (b == false) ()
      // or:
      // do () while (b == false)
    }
  } 
}
// 
// [Now checking: erasure]
// [check: erasure] New symbols: BoxedUnit UNIT runtime scala
// /tmp/fail.scala:4: error: 
// **** ERROR DURING INTERNAL CHECKING ****
// type mismatch;
//  found   : scala.runtime.BoxedUnit
//  required: Unit
//       while (b == false) ()
//       ^
// one error found
