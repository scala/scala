import Specializable._

class A[@specialized(Primitives) T](x: T) {
  def f1[@specialized(Primitives) U](x: T, y: U) = ((x, y))
  def f2[@specialized(Everything) U](x: T, y: U) = ((x, y))
  def f3[@specialized(Bits32AndUp) U](x: T, y: U) = ((x, y))
  def f4[@specialized(Integral) U](x: T, y: U) = ((x, y))
  def f5[@specialized(AllNumeric) U](x: T, y: U) = ((x, y))
  def f6[@specialized(BestOfBreed) U](x: T, y: U) = ((x, y))
  def f7[@specialized(Byte, Double, AnyRef) U](x: T, y: U) = ((x, y))
}
class B[@specialized(Everything) T] {
  def f1[@specialized(Primitives) U](x: T, y: U) = ((x, y))
  def f2[@specialized(Everything) U](x: T, y: U) = ((x, y))
  def f3[@specialized(Bits32AndUp) U](x: T, y: U) = ((x, y))
  def f4[@specialized(Integral) U](x: T, y: U) = ((x, y))
  def f5[@specialized(AllNumeric) U](x: T, y: U) = ((x, y))
  def f6[@specialized(BestOfBreed) U](x: T, y: U) = ((x, y))
  def f7[@specialized(Byte, Double, AnyRef) U](x: T, y: U) = ((x, y))
}
class C[@specialized(Bits32AndUp) T] {
  def f1[@specialized(Primitives) U](x: T, y: U) = ((x, y))
  def f2[@specialized(Everything) U](x: T, y: U) = ((x, y))
  def f3[@specialized(Bits32AndUp) U](x: T, y: U) = ((x, y))
  def f4[@specialized(Integral) U](x: T, y: U) = ((x, y))
  def f5[@specialized(AllNumeric) U](x: T, y: U) = ((x, y))
  def f6[@specialized(BestOfBreed) U](x: T, y: U) = ((x, y))
  def f7[@specialized(Byte, Double, AnyRef) U](x: T, y: U) = ((x, y))
}
class D[@specialized(Integral) T] {
  def f1[@specialized(Primitives) U](x: T, y: U) = ((x, y))
  def f2[@specialized(Everything) U](x: T, y: U) = ((x, y))
  def f3[@specialized(Bits32AndUp) U](x: T, y: U) = ((x, y))
  def f4[@specialized(Integral) U](x: T, y: U) = ((x, y))
  def f5[@specialized(AllNumeric) U](x: T, y: U) = ((x, y))
  def f6[@specialized(BestOfBreed) U](x: T, y: U) = ((x, y))
  def f7[@specialized(Byte, Double, AnyRef) U](x: T, y: U) = ((x, y))
}
class E[@specialized(AllNumeric) T] {
  def f1[@specialized(Primitives) U](x: T, y: U) = ((x, y))
  def f2[@specialized(Everything) U](x: T, y: U) = ((x, y))
  def f3[@specialized(Bits32AndUp) U](x: T, y: U) = ((x, y))
  def f4[@specialized(Integral) U](x: T, y: U) = ((x, y))
  def f5[@specialized(AllNumeric) U](x: T, y: U) = ((x, y))
  def f6[@specialized(BestOfBreed) U](x: T, y: U) = ((x, y))
  def f7[@specialized(Byte, Double, AnyRef) U](x: T, y: U) = ((x, y))
}
class F[@specialized(BestOfBreed) T] {
  def f1[@specialized(Primitives) U](x: T, y: U) = ((x, y))
  def f2[@specialized(Everything) U](x: T, y: U) = ((x, y))
  def f3[@specialized(Bits32AndUp) U](x: T, y: U) = ((x, y))
  def f4[@specialized(Integral) U](x: T, y: U) = ((x, y))
  def f5[@specialized(AllNumeric) U](x: T, y: U) = ((x, y))
  def f6[@specialized(BestOfBreed) U](x: T, y: U) = ((x, y))
  def f7[@specialized(Byte, Double, AnyRef) U](x: T, y: U) = ((x, y))
}
class G[@specialized(Byte, Double, AnyRef) T] {
  def f1[@specialized(Primitives) U](x: T, y: U) = ((x, y))
  def f2[@specialized(Everything) U](x: T, y: U) = ((x, y))
  def f3[@specialized(Bits32AndUp) U](x: T, y: U) = ((x, y))
  def f4[@specialized(Integral) U](x: T, y: U) = ((x, y))
  def f5[@specialized(AllNumeric) U](x: T, y: U) = ((x, y))
  def f6[@specialized(BestOfBreed) U](x: T, y: U) = ((x, y))
  def f7[@specialized(Byte, Double, AnyRef) U](x: T, y: U) = ((x, y))
}
