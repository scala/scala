import scala.collection.Iterable

object Test extends App {
  def assertHonorsIterableContract(i: Iterable[_]) = assert(i.size == i.iterator.size)

  assertHonorsIterableContract(<a/>.attributes)
  assertHonorsIterableContract(<a x=""/>.attributes)
  assertHonorsIterableContract(<a y={None}/>.attributes)
  assertHonorsIterableContract(<a y={None} x=""/>.attributes)
  assertHonorsIterableContract(<a a="" y={None} />.attributes)
  assertHonorsIterableContract(<a y={null:String}/>.attributes)
  assertHonorsIterableContract(<a y={null:String} x=""/>.attributes)
  assertHonorsIterableContract(<a a="" y={null:String} />.attributes)
}
