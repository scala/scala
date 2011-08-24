object Crash {
  def crash(value: Int): Unit =
      value match {
        case java.lang.Integer.MAX_VALUE => println("MAX_VALUE")
        case java.lang.Integer.MIN_VALUE => println("MIN_VALUE")
       }
}