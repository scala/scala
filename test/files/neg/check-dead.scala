package dummy

object Error {
    def soSorry(msg: String = "sorry"): Nothing =
        throw new Exception("we have a problem: "+msg)
}

class NoDeads {
    def x = synchronized { throw new Exception }
    def y[T](arg: T) = println("foo")
    def z = this.y(throw new Exception)

    def dummy1: Int = synchronized {
        val i = 10 + 2
        return i
    }
    def dummy1b: Int = synchronized {
        val i = 10 + 2
        i
    }

    def dummy2: String = Error.soSorry("we're dummies")
}

class Deads {
  def x1 = synchronized {
    throw new Exception
    5 * 5
  }
  def x2: Int = synchronized {
    throw new Exception
    return 5
  }
}