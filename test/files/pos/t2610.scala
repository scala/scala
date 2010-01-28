package mada; package defects; package tests

package object bbb {
    def bar = ()
    aaa.foo // value foo is not a member of package mada.defects.tests.aaa
}

package object aaa {
    def foo = ()
}

/* compiles successfully if placed here..
package object bbb {
    def bar = ()
    aaa.foo // value foo is not a member of package mada.defects.tests.aaa
}
*/