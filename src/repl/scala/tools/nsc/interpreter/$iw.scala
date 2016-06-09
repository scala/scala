package scala.tools.nsc.interpreter

// The REPL adds a magic import of "import scala.tools.nsc.interpreter.$iw"
// to increase the effective nesting level of the typechecker.
//
// `foo` will bind to `baz.foo` in this example:
//
// { import bar.foo; import interpreseter.$iw; import baz.foo; foo }
class $iw
