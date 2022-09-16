class C { def f(sb: StringBuilder): sb.type = sb }

// It's ok, if not specify a return type. 
class D extends C { override def f(sb: StringBuilder) = sb }