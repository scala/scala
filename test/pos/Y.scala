object test {

  def f(x: Object): java.lang.Object /* !!! Object */ = x.match;

}


