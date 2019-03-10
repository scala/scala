import java.util.Comparator

trait RDDLike[T] {
  def max(comp: Comparator[T]): T = {
    (1.0).asInstanceOf[T]
  }
}

class DoubleRDD extends RDDLike[java.lang.Double] { }
