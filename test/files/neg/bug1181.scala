package test

import scala.collection.immutable.Map

class CompilerTest(val valueList: List[Symbol]) {
	def buildMap(map: Map[Symbol, Symbol], keyList: List[Symbol], valueList: List[Symbol]): Map[Symbol, Symbol] = {
   (keyList, valueList) match {
     case (Nil, Nil) => map
     _ => buildMap(map.updated(keyList.head, valueList.head), keyList.tail, valueList.tail)
   }
  }
}
