

import annotation.static



class StaticInClass {
  @static val bar = 1
}


class NestedObjectInClass {
  object Nested {
    @static val blah = 2
  }
}


object NestedObjectInObject {
  object Nested {
    @static val succeed = 3
  }
}


object Conflicting {
  @static val bar = 1
}


class Conflicting {
  val bar = 45
}
