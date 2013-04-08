object Test {
  // crash
  def foo(a: Any) = { import a.{toString => toS}; toS }

  // okay
  def ok1(a: String) = { import a.{isInstanceOf => iio}; iio[String] }
  def ok2(a: Int) = { import a.{toInt => ti}; ti }
}
