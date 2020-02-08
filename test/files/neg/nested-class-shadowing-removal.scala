// scalac: -Werror -Xlint:deprecation -Xsource:3.0
//

trait Core {
  class Status
}

trait Ext extends Core {
  class Status extends super.Status
}
