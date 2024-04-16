//> using options -Werror -Xlint:deprecation
//

trait Core {
  class Status
}

trait Ext extends Core {
  class Status extends super.Status
}

trait AlsoNo extends Core {
  class Status
}

trait NotEven extends Core {
  class Status
  object Status
}

class NonStatus

trait WhatAbout extends Core {
  type Status = NonStatus
}

object Test extends App {
  val w = new WhatAbout {}
  val x = new w.Status
  println(x)
}
