//> using options -feature

trait T {
  def f(): scala.xxx.XXX[_, _] = ???
}

/*
t10938.scala:4: error: object xxx is not a member of package scala
  def f(): scala.xxx.XXX[_, _] = ???
                 ^
t10938.scala:4: warning: the existential type <error> forSome { type _$1; type _$2 }, which cannot be expressed by wildcards,  should be enabled
by making the implicit value scala.language.existentials visible.
This can be achieved by adding the import clause 'import scala.language.existentials'
or by setting the compiler option -language:existentials.
See the Scaladoc for value scala.language.existentials for a discussion
why the feature should be explicitly enabled.
  def f(): scala.xxx.XXX[_, _] = ???
                     ^
one warning found
one error found
*/
