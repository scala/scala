/*
 * scalac: -Xfatal-warnings -Xlint
 */
package t8013

// unsuspecting user of perverse macro
trait User {
  import Perverse.Impervolator
  val foo = "bar"
  Console println p"Hello, $foo"
}
