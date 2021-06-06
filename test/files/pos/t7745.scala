
package bug

import scala.language.implicitConversions

class Base[T]

class BaseOps[T] {
  type OpT[U] = Op[T, U]          // Fails below
  //type OpT[U] = List[U]         // OK
  //trait OpT[U] extends Op[T, U] // OK

  def op(tgt: OpTarget[OpT]) = tgt
}

object Base {
  implicit def baseOps[T](b: Base[T]): BaseOps[T] = new BaseOps[T]
}

class Op[A, B]

class OpTarget[TC[_]]

object OpTarget {
  implicit def apply[TC[_]](a: Any): OpTarget[TC] = new OpTarget[TC]
}

object TestBase {
  val baseOps = new BaseOps[String]
  baseOps.op(23) // OK in all cases

  val base = new Base[String]
  base.op(23)  // In the failing case:
  // found   : Int(23)
  // required: shapeless.OpTarget[[U]shapeless.Op[String,U]]
  //  base.op(23)
  //          ^
}
