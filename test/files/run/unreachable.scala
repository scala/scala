import scala.util.Random.nextInt
import scala.sys.error

object Test extends App {
  def unreachableNormalExit: Int = {
    return 42
    0
  }

  def unreachableIf: Int = {
    return 42
    if (nextInt % 2 == 0)
      0
    else
      1
  }

  def unreachableIfBranches: Int = {
    if (nextInt % 2 == 0)
      return 42
    else
      return 42

    return 0
  }

  def unreachableOneLegIf: Int = {
    if (nextInt % 2 == 0)
      return 42

    return 42
  }

  def unreachableLeftBranch: Int = {
    val result = if (nextInt % 2 == 0)
      return 42
    else
      42

    return result
  }

  def unreachableRightBranch: Int = {
    val result = if (nextInt % 2 == 0)
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
    val x = nextInt % 2
    x match {
      case 0 => return 0
      case 1 => return 1
      case -1 => return 2
    }
    3
  }

  def unreachableAfterSwitch: Int = {
    val x = nextInt % 2
    x match {
      case 0 => return 42
      case 1 => return 41 + x
      case -1 => return 43 + x
    }
    2
  }

  def check(f: Int) = assert(f == 42, s"Expected 42 but got $f")

  check(unreachableNormalExit)
  check(unreachableIf)
  check(unreachableIfBranches)
  check(unreachableOneLegIf)
  check(unreachableLeftBranch)
  check(unreachableRightBranch)
  check(unreachableTryCatchFinally)
  check(unreachableAfterTry)
  check(unreachableAfterCatch)
  check(unreachableAfterFinally)
  check(unreachableSwitch)
  check(unreachableAfterSwitch)
}
