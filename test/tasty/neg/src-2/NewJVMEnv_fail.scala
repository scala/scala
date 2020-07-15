package tastytest

import printing._

object NewJVMEnv {

  new Env.JVMEnv {} // error: class type required but tastytest.printing.AnyHash with tastytest.printing.AnyClassName found

}
