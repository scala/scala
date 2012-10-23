object t1 {
	case object Const {
	}

	class Var
	{

} // missing brace

object t2 {
	case class Const() {
	}

	class Var
	{

} // missing brace

object t3 {
	final case class Const() {
	}

	class Var
	{

} // missing brace
