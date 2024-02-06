package tastytest

import lib.Bounds

object TestBounds extends scala.App {
  Bounds.m_ARRAY(Array(23)) // error
  Bounds.m_ARRAY(Array("abc")) // error
  Bounds.m(23) // error
  Bounds.m("abc") // error
}
