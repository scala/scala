trait SizeChangeEvent { protected var end: Int = 1 }
class BackedUpListIterator[E](override protected var end: Int) extends SizeChangeEvent
