//> using options -deprecation -Werror

class Test {
  ("": Any).##()
  ("": AnyRef).##()
  ("": Object).##()

  def meth() = ""
  meth // warn, auto-application (of nilary methods) is deprecated
}
