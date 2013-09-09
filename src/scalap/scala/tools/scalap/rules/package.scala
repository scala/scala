package scala.tools.scalap

package object rules {
  // make some language features in this package compile without warning
  implicit def postfixOps = scala.language.postfixOps
}
