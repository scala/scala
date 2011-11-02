object Broken {
  private var map = Map[Class[_], String]()
  
  def addToMap(c : Class[_], s : String) = map += (c -> s)
  def fetch(c : Class[_]) = map(c)
}

object Works {
  private var map = Map[Class[_], String]()
  
  def addToMap(c : Class[_], s : String) = map += ((c, s))
  def fetch(c : Class[_]) = map(c)
}

object Works2 {
  private var map = Map[Class[_], String]()
  
  def addToMap(c : Class[_], s : String) = map += ((c : Class[_]) -> s)
  def fetch(c : Class[_]) = map(c)
}