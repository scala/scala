package tastytest

import lib.{JavaEnum, ScalaEnum}

object TestEnum {

  val i = scala.util.Random.nextInt(3)
  JavaEnum.values()(i) match { // error
    case a @ JavaEnum.A => assert(a.name == "A" && a.ordinal == i)
    // case b @ JavaEnum.B => assert(b.name == "B" && b.ordinal == i)
    case c @ JavaEnum.C => assert(c.name == "C" && c.ordinal == i)
  }

  ScalaEnum.values(i) match { // error
    case a @ ScalaEnum.A => assert(a.toString == "A" && a.ordinal == i)
    // case b @ ScalaEnum.B => assert(b.toString == "B" && b.ordinal == i)
    case c @ ScalaEnum.C => assert(c.toString == "C" && c.ordinal == i)
  }

}
