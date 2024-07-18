//
//> using options -Wunused:privates -Werror
//
package com

package example {
  private object Test {
    def foo: String = "foo"
    def foobar: String = example.bar   // not a usage
    def detest = example.Detest        // not a usage
  }
}
package object example {
  private def bar: String = "bar"
  private[example] def barNone: String = "bar"   // not unqualified private

  private object Detest
}
