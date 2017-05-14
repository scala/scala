/*
 * This tests that the predicate of dropWhile is only evaluated as often as needed, see https://github.com/scala/bug/issues/5387
 */
import scala.collection.immutable.ListMap
object Test extends App{
  val subject = ListMap(1->1,2->2,3->3,4->4,5->5)
  val result  = ListMap(3->3,4->4,5->5)
  assert( result == subject.dropWhile{
       case (key, value) => {
         assert( key <= 3, "predicate evaluated more often than needed, key "+key )
         key < 3
       }
    }
  )
}
