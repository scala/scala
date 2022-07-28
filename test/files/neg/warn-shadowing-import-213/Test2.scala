// scalac: -Xsource:2.13 -Werror

package b
import b._ // no warning, imported b.O is the same as package member b.O

class D {
  def t = O.b
}
