package scala.xml.dtd.impl

import scala.collection._

abstract class S2 {
  val lang: WordExp
  type __labelT = lang._labelT

  var deltaq: Array[__labelT] = _
  def delta1  = immutable.Map(deltaq.zipWithIndex: _*)
}
