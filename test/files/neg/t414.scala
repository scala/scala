case class Empty[a]() extends IntMap[a];
case class Node[a](left: IntMap[a], keyVal: Pair[Int, a], right: IntMap[a]) extends IntMap[a];
abstract class IntMap[a] {
        def lookup(key: Int): a = this match {
                case Empty =>
                        error("clef inexistante")
                case _ =>
        };

};
