import scala.language.experimental.macros

object Test extends App {
  def one(x: Int = 2, y: Int = -40): Unit = macro Impls.one
  one(2, -40)
  one(y = -40, x = 2)
  one(x = 2, y = -40)
  one(x = 100)
  one(y = 100)
  one(100)
  one()
  var qualone = this
  qualone.one(2, -40)
  qualone.one(y = -40, x = 2)
  qualone.one(x = 2, y = -40)
  qualone.one(x = 100)
  qualone.one(y = 100)
  qualone.one(100)
  qualone.one()

  def onezero(x: Int = 2, y: Int = -40)(z: Int, w: Int): Unit = macro Impls.onezero
  onezero(2, -40)(1, 2)
  onezero(y = -40, x = 2)(3, 4)
  onezero(x = 2, y = -40)(5, 6)
  onezero(x = 100)(7, 8)
  onezero(y = 100)(9, 10)
  onezero(100)(11, 12)
  onezero()(13, 14)
  var qualonezero = this
  qualonezero.onezero(2, -40)(15, 16)
  qualonezero.onezero(y = -40, x = 2)(17, 18)
  qualonezero.onezero(x = 2, y = -40)(19, 20)
  qualonezero.onezero(x = 100)(21, 22)
  qualonezero.onezero(y = 100)(23, 24)
  qualonezero.onezero(100)(25, 26)
  qualonezero.onezero()(27, 28)

  def zeroone(x: Int, y: Int)(z: Int = 2, w: Int = -40): Unit = macro Impls.zeroone
  zeroone(1, 2)(2, -40)
  zeroone(3, 4)(w = -40, z = 2)
  zeroone(5, 6)(z = 2, w = -40)
  zeroone(7, 8)(z = 100)
  zeroone(9, 10)(w = 100)
  zeroone(11, 12)(100)
  zeroone(13, 14)()
  var qualzeroone = this
  qualzeroone.zeroone(15, 16)(2, -40)
  qualzeroone.zeroone(17, 18)(w = -40, z = 2)
  qualzeroone.zeroone(19, 20)(z = 2, w = -40)
  qualzeroone.zeroone(21, 22)(z = 100)
  qualzeroone.zeroone(23, 24)(w = 100)
  qualzeroone.zeroone(25, 26)(100)
  qualzeroone.zeroone(27, 28)()

  def oneone(x: Int = 2, y: Int = -40)(z: Int = 2, w: Int = -40): Unit = macro Impls.oneone
  oneone(2, -40)(2, -40)
  oneone(y = -40, x = 2)(w = -40, z = 2)
  oneone(x = 2, y = -40)(z = 2, w = -40)
  oneone(x = 100)(z = 100)
  oneone(y = 100)(w = 100)
  oneone(100)(100)
  oneone()()
  var qualoneone = this
  qualoneone.oneone(2, -40)(2, -40)
  qualoneone.oneone(y = -40, x = 2)(w = -40, z = 2)
  qualoneone.oneone(x = 2, y = -40)(z = 2, w = -40)
  qualoneone.oneone(x = 100)(z = 100)
  qualoneone.oneone(y = 100)(w = 100)
  qualoneone.oneone(100)(100)
  qualoneone.oneone()()
}