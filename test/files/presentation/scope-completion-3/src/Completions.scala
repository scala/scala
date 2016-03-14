package test

/* check availability of members defined locally and in hierarchy */

abstract class Base1 {

  type tb1 = Int
  val vb1 = 0
  var rb1 = 0 
  def fb1 = 0
  class Cb1
  object Ob1

  private type tb2 = Int
  private val vb2 = 0
  private var rb2 = 0
  private def fb2 = 0
  private class Cb2
  private object Ob2

  type tb3
  val vb3: Int
  var rb3: Int
  def fb3: Int
}

trait Trait1 {

  type tt1 = Int
  val vt1 = 0
  var rt1 = 0
  def ft1 = 0
  class Ct1
  object Ot1

  private type tt2 = Int
  private val vt2 = 0
  private var rt2 = 0
  private def ft2 = 0
  private class Ct2
  private object Ot2

  type tt3
  val vt3: Int
  var rt3: Int
  def ft3: Int
}

class Completion1 extends Base1 with Trait1 {

  type tc1 = Int
  val vc1 = 0
  var rc1 = 0
  def fc1 = 0
  class Cc1
  object Oc1

  private type tc2 = Int
  private val vc2 = 0
  private var rc2 = 0
  private def fc2 = 0
  private class Cc2
  private object Oc2

  override type tb3 = Int
  override val vb3 = 12
  override var rb3 = 12
  override def fb3 = 12
  
  override type tt3 = Int
  override val vt3 = 12
  override var rt3 = 12
  override def ft3 = 12

  /*_*/
}

object Completion2 extends Base1 with Trait1 {

  type to1 = Int
  val vo1 = 0
  var ro1 = 0
  def fo1 = 0
  class Co1
  object Oo1

  private type to2 = Int
  private val vo2 = 0
  private var ro2 = 0
  private def fo2 = 0
  private class Co2
  private object Oo2

  override type tb3 = Int
  override val vb3 = 12
  override var rb3 = 12
  override def fb3 = 12
  
  override type tt3 = Int
  override val vt3 = 12
  override var rt3 = 12
  override def ft3 = 12
  
  /*_*/
}

