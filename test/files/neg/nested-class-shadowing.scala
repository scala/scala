// scalac: -Werror -Xlint:deprecation
//

trait Core {
  class Status
}

trait Ext extends Core {
  class Status extends super.Status
}
