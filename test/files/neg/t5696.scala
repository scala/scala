class TestApply1 {
  class G(y: Int) {
    def apply(x: Int) = 1
  }

  new G(1)(2) {}
}

class TestApply2 {
  class G {
    def apply(x: Int) = 1
  }

  new G()(2) {}
}

class TestApply3 {
  class G[X] {
    def apply(x: Int) = 1
  }

  new G[Int]()(2) {}
}

class TestApply4 {
  class G[X] {
    def apply(x: Int)(y: Int) = 1
  }

  new G[Int]()(2)(3) {}
}

class TestApply5 {
  class G[X]()() {
    def apply(x: Int) = 1
  }

  new G[Int]()()(2) {}
}

class TestApply6 {
  class G(y: Int) {
    def apply(x: Int) = 1
  }

  object x extends G(1)(2) {}
}
