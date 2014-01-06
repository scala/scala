class Foo {
  def toInt = 12
}
case class Bar( fooBar : Int )

// spurious "erroneous or inaccessible type" error in 2.10.1
class Test {
  var fooBar : Foo = null
  def build = Bar(
    fooBar = foBar.toInt
  )
}
