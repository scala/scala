class Test {

  def byNameFunc(f: (=> (() => Any)) => Any): Unit = ()

  def test = {
    // "value apply is not a member of => () => Any"
    byNameFunc(z => z())
    // okay
    byNameFunc(z => z.apply())
    byNameFunc(z => {val f = z; f()})
  }
}
