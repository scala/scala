//> using options -deprecation -Xsource:3-cross -Wconf:cat=scala3-migration:w -Werror

object NameShadowing {
  class A { class X }
  class B extends A { class X; def f = new X }
}
