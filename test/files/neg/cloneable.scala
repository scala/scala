
//> using options -Werror -Xlint:cloneable
//> using test.options --recompile=-Wconf:cat=lint-cloneable:s

class Base extends Cloneable

object X extends Base

class Y extends Base
