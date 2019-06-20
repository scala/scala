class Base {
    private[this] var counts: Array[Long] = _
}

class Cover extends Base {
    private[this] var counts: Array[Int] = _
}

class LocalImpl extends Cover
