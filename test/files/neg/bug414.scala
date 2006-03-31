case class Empty[a] extends IntMap[a];
case class Node[a](left: IntMap[a], keyVal: Pair[int, a], right: IntMap[a]) extends IntMap[a];
abstract class IntMap[a] {
        def lookup(key: int): a = this match {
                case Empty =>
                        error("clef inexistante")
                case _ =>
        };

};
