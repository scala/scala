class Rational(x: Int, y: Int) {
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b);
  private def g = gcd(x, y);
  def numer = x / g;
  def denom = y / g;
  def add(r: Rational) =
    new Rational(
      numer * r.denom + r.numer * denom,
      denom * r.denom);
  def sub(r: Rational) =
    new Rational(
      numer * r.denom - r.numer * denom,
      denom * r.denom);
  def mul(r: Rational) =
    new Rational(
      numer * r.numer,
      denom * r.denom);
  def equal(r: Rational) =
    new Rational(
      numer * r.denom,
      denom * r.numer);

  def asString: String = numer + "/" + denom;
  override def toString(): String = numer + "/" + denom
}
