trait SizeChangeEvent { protected var end: Int }
class BackedUpListIterator[E](override protected var end: Int) extends SizeChangeEvent
