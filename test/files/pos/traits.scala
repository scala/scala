object Test {
  type Color = Int
  trait Shape {
    override def equals(other: Any) = true
  }
  trait Bordered extends Shape {
    val thickness: Int
    override def equals(other: Any) = other match {
      case that: Bordered => this.thickness == that.thickness
      case _ => false
    }
  }
  trait Colored extends Shape {
    val color: Color
    override def equals(other: Any) = other match {
      case that: Colored => this.color == that.color
      case _ => false
    }
  }
  trait BorderedColoredShape extends Shape with Bordered with Colored {
    override def equals(other: Any) = other match {
      case that: BorderedColoredShape => (
	super.equals(that) && 
        super[Bordered].equals(that) && 
        super[Colored].equals(that))
      case _ => false
    }
  }

  val bcs1 = new BorderedColoredShape {
    val thickness = 1
    val color = 0
  }
  val bcs2 = new BorderedColoredShape {
    val thickness = 2
    val color = 0
  }
  Console.println(bcs1 == bcs1)
  Console.println(bcs1 == bcs2)
}
