abstract class IntMap[a] {
    def lookup(key: Int): a = match {
        case Empty() => error("key " + key + " not found")
        case _ => error("ok")
    }
}

case class Empty[a]() extends IntMap[a];

object Test {

    def main(args: Array[String]): Unit = {
        val key = 2000;
        val map: IntMap[String] = new Empty[String];

        System.out.print("lookup(" + key + ") = ");
        try {
            System.out.println(map.lookup(key));
        } except {
            case e => System.out.println(e.getMessage());
        }
    }

}

