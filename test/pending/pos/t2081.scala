class RichInt(n: Int) {
  def days = 1000*60*60*24*n
}

implicit def RichInt(n: Int): RichInt = new RichInt(n)

10.days
