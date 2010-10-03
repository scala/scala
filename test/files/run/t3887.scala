object Test {
    def main(args: Array[String]) {
        assert( matchPair(1) )
        assert( !matchPair(2) )
    }

    def matchPair(i: Int) = {
        (i, "abc") match {
            case this.option1 => true
            case _ => false
        }
    }

    val option1: (Int, String) = (1, "abc")

}
