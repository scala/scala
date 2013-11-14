class WordExp {
  abstract class Label
  type _labelT <: Label
}

import scala.collection._

abstract class S2 {
  val lang: WordExp
  type __labelT = lang._labelT

  var deltaq: Array[__labelT] = _
  def delta1  = immutable.Map(deltaq.zipWithIndex: _*)
}
