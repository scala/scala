object m1 {

  def gcd(x: Int, y: Int): Int =
    if (y == 0) x
    else gcd(y, x % y);

  gcd(14, 21);

  def smallestDivisor(n: Int) = {
    def findDivisor(d: Int): Int =
      if (d * d > n) n
      else if (n % d == 0) d
      else findDivisor(d + 1);
    findDivisor(2);
  }

  def isPrime(n: Int) = smallestDivisor(n) == n;
}
