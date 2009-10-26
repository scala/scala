
object InvestmentType extends Enumeration {
  val Debt = Value("DEBT")
  val Future = Value("FUTURE")
  val Equity = Value("EQUITY")
}

object Test {
  def main(args: Array[String]) = {
    val buf = new java.io.ByteArrayOutputStream
    val oos = new java.io.ObjectOutputStream(buf)
    InvestmentType.values.foreach {i => oos.writeObject(i)}
    oos.flush
    oos.close
    val ois = new java.io.ObjectInputStream(
                new java.io.ByteArrayInputStream(buf.toByteArray))
    var obj: Object = null
    foreach(ois) { obj =>
      obj match {
        case InvestmentType.Debt => println("got " + obj)
        case InvestmentType.Equity => println("got " + obj)
        case InvestmentType.Future => println("got " + obj)
        case _ => println("unknown: " + obj + " of: " + obj.getClass)
      }
    }
  }

  def foreach(os: java.io.ObjectInputStream)(f: Object => Unit) {
    try {
      val obj = os.readObject
      if (obj != null) {
        f(obj)
        foreach(os)(f)
      }
    } catch {
      case e: java.io.EOFException => //IGNORE
    }
  }
}
