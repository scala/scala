package scala.collection.parallel



import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ArraySeq
import scala.collection.generic.Sizing



package object mutable {

  /* hack-arounds */

  private[mutable] class ExposedArrayBuffer[T] extends ArrayBuffer[T] with Sizing {
    def internalArray = array
    def setInternalSize(s: Int) = size0 = s
    override def sizeHint(len: Int) = { // delete once we start using 2.8.RC1+
      if (len > size && len >= 1) {
        val newarray = new Array[AnyRef](len)
        Array.copy(array, 0, newarray, 0, size0)
        array = newarray
      }
    }
  }

  private[mutable] class ExposedArraySeq[T](arr: Array[AnyRef], sz: Int) extends ArraySeq[T](sz) {
    override val array = arr
    override val length = sz
  }

}