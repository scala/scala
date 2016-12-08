

class C(i: Int)(implicit j: Int)(implicit k: Int)(n: Int) {
  def f = n
}

class D(private implicit val i: Int)(implicit s: String)
