package test

class Thing {
  def info: Info[this.type] = InfoRepository.getInfo(this)
  def info2: Info[this.type] = {
    def self: this.type = this
    InfoRepository.getInfo(self)
  }
}

trait Info[T]
case class InfoImpl[T](thing: T) extends Info[T]

object InfoRepository {
  def getInfo(t: Thing): Info[t.type] = InfoImpl(t)
}
