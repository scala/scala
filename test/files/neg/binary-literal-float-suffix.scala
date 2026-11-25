trait T {
  def f = 0b101f
  def F = 0b101F
  def d = 0b10d
  def D = 0b10D
  def e = 0b11e2
  def E = 0b11E2
  def negExp = 0b1e-1
  def digit = 0b2

  def long = 0b101L       // ok
  def hexF = 0x1f         // ok, 31
  def hexD = 0x1d         // ok, 29
  def hexE = 0x1e2        // ok, 482
  def dec = (101f, 11e2, 10d, 1e-1) // ok
}
