package scala.collection.jcl

object Conversions {
  implicit def convertSet[T](set : java.util.Set[T]) = Set(set)
  implicit def convertList[T](set : java.util.List[T]) = Buffer(set)
  implicit def convertSortedSet[T](set : java.util.SortedSet[T]) = SortedSet(set)
  implicit def convertMap[T,E](set : java.util.Map[T,E]) = Map(set)
  implicit def convertSortedMap[T,E](set : java.util.SortedMap[T,E]) = SortedMap(set)

  implicit def unconvertSet[T](set : SetWrapper[T]) = set.underlying
  implicit def unconvertCollection[T](set : CollectionWrapper[T]) = set.underlying
  implicit def unconvertList[T](set : BufferWrapper[T]) = set.underlying
  implicit def unconvertSortedSet[T](set : SortedSetWrapper[T]) = set.underlying
  implicit def unconvertMap[T,E](set : MapWrapper[T,E]) = set.underlying
  implicit def unconvertSortedMap[T,E](set : SortedMapWrapper[T,E]) = set.underlying

}