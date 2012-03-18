// TEST1: Inherit from multiple classes
trait A {
  /**
   * Hello 1 comment
   */
  def hello1 = 0
}

trait B {
  /**
   * Hello 2 comment
   */
  def hello2 = 1
}

trait C extends B

class D extends A with C {
  /**
   * Inherited: @inheritdoc
   */
  override def hello1 = super.hello2

  /**
   * Inherited: @inheritdoc
   */
  override def hello2 = super.hello1
}

// TEST2: Invalid inherit: no parents
trait E {
  /**
   * @inheritdoc
   */
  def whereDidThisComeFrom
}

// TEST3: Invalid inherit, but other parents present
trait F extends E {
  /**
   * @inheritdoc
   */
  def howAboutThis
}


// TEST4: Inherit from something that inherits: inherit should propagate
trait G extends D {
  /**
   * @inheritdoc
   */
  override def hello1 = 13

  /**
   * @inheritdoc
   */
  override def hello2 = 14
}

// TEST5: Inherit missing parameters
trait H extends G {
  /**
   * Missing params
   * @throws HelloException @inheritdoc
   * @todo @inheritdoc
   */
  override def hello1 = 15
}

// TEST6: Inherit from something that inherits in the usecase
trait I extends G {
  /**
   * @inheritdoc
   * @usecase def hello1(i: Int)
   *   @inheritdoc
   */
  override def hello1 = 13
}