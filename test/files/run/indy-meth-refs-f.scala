// scalac: -Ydelambdafy:method-ref
object Test {
  def anyA(f: Any => Any)     = ()
  def anyB(f: Any => Boolean) = ()
  def anyI(f: Any => Int)     = ()

  def objA(f: Object => Any)           = ()
  def objB(f: Object => Boolean)       = ()
  def objI(f: Object => Int)           = ()

  def strS(f: String => String) = ()

  def arrII(f: Array[Int] => Int)       = ()
  def arrSI(f: Array[String] => Int)    = ()
  def arrSS(f: Array[String] => String) = ()

  def boolB(f: Boolean => Boolean) = ()

  def intI(f: Int => Int)     = ()
  def intB(f: Int => Boolean) = ()

  class StrVC(val x: String) extends AnyVal {
    def unwrap = x
    def bang   = new StrVC(x + "!")
  }
  class BoolVC(val x: Boolean) extends AnyVal {
    def unwrap = x
    def not    = new BoolVC(!x)
  }
  class IntVC(val x: Int) extends AnyVal {
    def unwrap   = x
    def inc      = new IntVC(x + 1)
    def isZero   = x == 0
    def isZeroVC = new BoolVC(x == 0)
  }

  def mkStrVC(x: String): StrVC    = new StrVC(x)
  def mkBoolVC(x: Boolean): BoolVC = new BoolVC(x)
  def mkIntVC(x: Int): IntVC       = new IntVC(x)

  def strVC1(f: StrVC => String) = ()
  def strVC2(f: StrVC => StrVC)  = ()
  def strVC3(f: String => StrVC) = ()

  def boolVC1(f: BoolVC => Boolean) = ()
  def boolVC2(f: BoolVC => BoolVC)  = ()
  def boolVC3(f: Boolean => BoolVC) = ()

  def intVC1(f: IntVC => Int)     = ()
  def intVC2(f: IntVC => IntVC)   = ()
  def intVC3(f: Int => IntVC)     = ()
  def intVC4(f: IntVC => Boolean) = ()

  def vcs1(f: IntVC => BoolVC) = ()

  def main(args: Array[String]): Unit = {
    anyB("" == _)
    anyB("" != _)
    anyB(_.isInstanceOf[String])
    anyA(_.asInstanceOf[String])
    anyI(_.##)

    objB("" eq _)
    objB("" ne _)
    objB("" == _)
    objB("" != _)
    objB(_.isInstanceOf[String])
    objA(_.asInstanceOf[String])
    objA("".synchronized(_))

    strS("" + _)

    arrII(_.length)
    arrII(xs => xs(0))
    arrSI(_.length)
    arrSS(xs => xs(0))

    //boolB(true eq _) // the result type of an implicit conversion must be more specific than AnyRef  ¯\_(ツ)_/¯
    //boolB(true ne _)
    boolB(!_)
    boolB(true || _)
    boolB(true && _)
    boolB(true | _)
    boolB(true & _)
    boolB(true ^ _)

    //intB(1 eq _)
    //intB(1 ne _)
    intI(1 + _)
    intI(1 - _)
    intI(1 * _)
    intI(1 / _)
    intI(1 % _)
    intB(1 < _)
    intB(1 <= _)
    intB(1 > _)
    intB(1 >= _)
    intI(1 ^ _)
    intI(1 & _)
    intI(1 << _)
    intI(1 >>> _)
    intI(1 >> _)
    intI(_.toInt)
    intI(i => -i)
    intI(i => ~i)

    strVC1(_.unwrap)
    strVC2(_.bang)
    strVC3(mkStrVC(_))

    boolVC1(_.unwrap)
    boolVC2(_.not)
    boolVC3(mkBoolVC(_))

    intVC1(_.unwrap)
    intVC2(_.inc)
    intVC3(mkIntVC(_))
    intVC4(_.isZero)

    vcs1(_.isZeroVC)
  }
}
