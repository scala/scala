import scala.sys.error

class Foo_1 {
  def unreachableNormalExit: Int = {
    return 42
    0
  }

  def unreachableIf: Int = {
    return 42
    if (util.Random.nextInt % 2 == 0)
      0
    else
      1
  }

  def unreachableIfBranches: Int = {
    if (util.Random.nextInt % 2 == 0)
      return 42
    else
      return 42

    return 0
  }

  def unreachableOneLegIf: Int = {
    if (util.Random.nextInt % 2 == 0)
      return 42

    return 42
  }

  def unreachableLeftBranch: Int = {
    val result = if (util.Random.nextInt % 2 == 0)
      return 42
    else
      42

    return result
  }

  def unreachableRightBranch: Int = {
    val result = if (util.Random.nextInt % 2 == 0)
      42
    else
      return 42

    return result
  }

  def unreachableTryCatchFinally: Int = {
    return 42
    try {
      return 0
    } catch {
      case x: Throwable => return 1
    } finally {
      return 2
    }
    return 3
  }

  def unreachableAfterTry: Int = {
    try {
      return 42
    } catch {
      case x: Throwable => return 2
    }
    return 3
  }

  def unreachableAfterCatch: Int = {
    try {
      error("haha")
    } catch {
      case x: Throwable => return 42
    }
    return 3
  }

  def unreachableAfterFinally: Int = {
    try {
      return 1
    } catch {
      case x: Throwable => return 2
    } finally {
      return 42
    }
    return 3
  }

  def unreachableSwitch: Int = {
  	return 42
    val x = util.Random.nextInt % 2
    x match {
      case 0 => return 0
      case 1 => return 1
      case _ => error("wtf")
    }
    2
  }

  def unreachableAfterSwitch: Int = {
    val x = util.Random.nextInt % 2
    x match {
      case 0 => return 42
      case 1 => return 41 + x
      case _ => error("wtf")
    }
    2
  }
}
