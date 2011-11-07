package test

trait Map[A, +B] {
  def plus(key: A): MapTo = new MapTo(key)
    
  class MapTo(key: A) {
    def arrow [B1 >: B](value: B1) = null
  }
}

