package test

trait BaseTrait {
  /** Some random integer. */
  def someInt: Int

  /** Another random integer. */
  val anotherInt: Int

  /** More integers, and mutable! */
  var yetMoreInt: Int

  /** The last integer. */
  def theLastInt: Int
}

class BaseClass extends BaseTrait {
  /** A doohickey. */
  def doohickey: AnyRef

  /** A whatzit. */
  val whatzit: AnyRef

  /** A fiddle. */
  var fiddle: AnyRef

  /** A surprise. */
  def surprise: AnyRef
}

class Test extends BaseClass {
  /**@inheritdoc */
  val someInt    : Int = 7
  /**@inheritdoc */
  val anotherInt : Int = 77
  /**@inheritdoc */
  var yetMoreInt : Int = 777
  /**@inheritdoc */
  var theLastInt : Int = 7777

  /**@inheritdoc */
  val doohickey : AnyRef = new AnyRef
  /**@inheritdoc */
  val whatzit   : AnyRef = new AnyRef
  /**@inheritdoc */
  var fiddle    : AnyRef = new AnyRef
  /**@inheritdoc */
  var surprise  : AnyRef = new AnyRef
}
