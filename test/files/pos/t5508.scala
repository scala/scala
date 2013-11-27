package TestTestters

trait Test1 {
  private[this] var _st : Int = 0
  def close : PartialFunction[Any,Any] = {
    case x : Int =>
      _st = identity(_st)
  }
}

object Base1 {
  trait Test2 {
    private[this] var _st : Int = 0
    def close : PartialFunction[Any,Any] = {
      case x : Int =>
  _st = identity(_st)
    }
  }
}

class Test3 {
  private[this] var _st : Int = 0
  def close : PartialFunction[Any,Any] = {
    case x : Int =>
      _st = 1
  }
}

object Base2 {
  class Test4 {
    private[this] var _st : Int = 0
    def close : PartialFunction[Any,Any] = {
      case x : Int =>
  _st = 1
    }
  }
}

class Base3 {
  trait Test5 {
    private[this] var _st : Int = 0
    def close : PartialFunction[Any,Any] = {
      case x : Int =>
  _st = 1
    }
  }
}

object Base4 {
  trait Test6 {
    private[this] var _st : Int = 0
    def close : PartialFunction[Any,Any] = {
      case x : Int => ()
    }
  }
}

object Base5 {
  trait Test7 {
    private[this] var _st : Int = 0
    def close = () => {
      _st = 1
    }
  }
}

object Base6 {
  class Test8 {
    private[this] var _st : Int = 0
    def close = () => {
      _st = 1
    }
  }
}

object Base7 {
  trait Test9 {
    var st : Int = 0
    def close = () => {
      st = 1
    }
  }
}
