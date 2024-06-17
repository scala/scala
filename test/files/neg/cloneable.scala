
//> using options -Werror -Xlint:cloneable
//> using retest.options -Wconf:cat=lint-cloneable:s

class Base extends Cloneable

object X extends Base

class Y extends Base
