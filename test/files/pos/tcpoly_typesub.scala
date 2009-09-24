// contributed by Lauri Alanko
trait TypeSub {
  type l
  type u
  def castSub[f[+x]](fl : f[l]) : f[u]
  def castSuper[f[-x]](fu : f[u]) : f[l] = {
    type c[+y] = f[y] => f[l]
    castSub[c]{ fl : f[l] => fl }(fu)
  }
  def castValue[t](lt : l with t) : u with t = {
    type c[+y] = y with t
    castSub[c](lt)
  }
}
