package tastytest

import printing._

/**This is staying here until type members of a package display with a path prefix, not a projection
 */
object NewJVMEnv {

  new Env.JVMEnv {} // error: class type required but tastytest.printing#AnyHash with tastytest.printing#AnyClassName found

}
