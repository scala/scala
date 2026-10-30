//> using options -Werror

// `M` is not completed yet when `t` is typed, so `L` has no known children yet.
object Local {
  def before = {
    class L
    trait T
    def t(x: L) = x.isInstanceOf[T]
    class M extends L with T
    t(new M)
  }
  def after = {
    class L
    trait T
    class M extends L with T
    def t(x: L) = x.isInstanceOf[T]
    t(new M)
  }
}
