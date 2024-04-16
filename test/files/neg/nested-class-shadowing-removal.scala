//> using options -Xsource:3
//

trait Core {
  class Status
}

trait Ext extends Core {
  class Status extends super.Status
}
