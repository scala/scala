import language._

object Foo {
  object I { 
    def +(other: I.type) : Unit = ()
  }
  object BI {
    def +(other: BI.type): Unit = ()
  }
  implicit def I2BI(i: I.type): BI.type = BI
  I.+(BI)
}
