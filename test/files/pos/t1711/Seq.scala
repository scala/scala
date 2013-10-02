package com

object Sequence {

  def filteringFunction[V](filter: V => Boolean): List[V] => List[V] = {
    def include(v: V) =
      filter(v)
    (l: List[V]) => l.filter(include)
  }

}

