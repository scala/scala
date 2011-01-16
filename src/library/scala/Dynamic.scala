package scala

trait Dynamic {
  def _select_ (name: String): Dynamic
  def _invoke_ (name: String)(args: Any*): Dynamic

  def _invoke_0(name: String)(): Dynamic = _invoke_ (name)()
  def _invoke_1(name: String)(arg1: Any): Dynamic = _invoke_ (name) (arg1)
  def _invoke_2(name: String)(arg1: Any, arg2: Any): Dynamic = _invoke_ (name)(arg1, arg2)
  def _invoke_3(name: String)(arg1: Any, arg2: Any, arg3: Any): Dynamic = _invoke_ (name)(arg1, arg2, arg3)
  def _invoke_4(name: String)(arg1: Any, arg2: Any, arg3: Any, arg4: Any): Dynamic = _invoke_ (name)(arg1, arg2, arg3, arg4)
  def _invoke_5(name: String)(arg1: Any, arg2: Any, arg3: Any, arg4: Any, arg5: Any): Dynamic = _invoke_ (name)(arg1, arg2, arg3, arg4, arg5)
  def _invoke_6(name: String)(arg1: Any, arg2: Any, arg3: Any, arg4: Any, arg5: Any, arg6: Any): Dynamic = _invoke_ (name)(arg1, arg2, arg3, arg4, arg5, arg6)
  def _invoke_7(name: String)(arg1: Any, arg2: Any, arg3: Any, arg4: Any, arg5: Any, arg6: Any, arg7: Any): Dynamic = _invoke_ (name)(arg1, arg2, arg3, arg4, arg5, arg6, arg7)

  def typed[T]: T = asInstanceOf[T]
}

