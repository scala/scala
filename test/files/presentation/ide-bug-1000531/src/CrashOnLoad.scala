/** When this files is opened within the IDE, a typing error is reported. */
class A[B] extends java.lang.Iterable[B] {
  import scala.collection.JavaConversions._
  def iterator = Iterator.empty 
  
  iterator. /*!*/
}