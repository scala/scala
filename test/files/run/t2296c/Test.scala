package test

import bug.Global

object Test {
  def main(args: Array[String]): Unit = {
    val m = new Main()
    m.init()
    m.start()
  }
}

class Main extends Outer {
  m_glob = new Global()
}
