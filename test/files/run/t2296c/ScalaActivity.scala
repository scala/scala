package test

import bug.Display
import bug.action.Action

abstract class Outer extends Display {

  def init() {
    m_glob.putAction(ScalaActivity)
  }

  object ScalaActivity extends Action {
    def run(v: Int) {
      val testSet = List(1,2,3)
      testSet.map(p => m_glob.items(p)) // crash with illegal access
    }
  }
}
