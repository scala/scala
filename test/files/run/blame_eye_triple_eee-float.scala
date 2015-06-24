object Test extends App {
  import Float.NaN

  // NaN must not equal NaN no matter what optimizations are applied
  // All the following will seem redundant, but to an optimizer
  // they can appear different

  val x = NaN

  if (NaN == NaN)
    println("if (NaN == NaN) is broken")
  else
    println("if (NaN == NaN) is good")

  if (x == x)
    println("if (x == x) is broken")
  else
    println("if (x == x) is good")

  if (x == NaN)
    println("if (x == NaN) is broken")
  else
    println("if (x == NaN) is good")

  if (NaN != NaN)
    println("if (NaN != NaN) is good")
  else
    println("if (NaN != NaN) broken")

  if (x != x)
    println("if (x != x) is good")
  else
    println("if (x != x) broken")

  if (NaN != x)
    println("if (NaN != x) is good")
  else
    println("if (NaN != x) is broken")

  x match {
    case 0.0f => println("x matched 0!")
    case NaN => println("x matched NaN!")
    case _ => println("x matching was good")
  }

  NaN match {
    case 0.0f => println("NaN matched 0!")
    case NaN => println("NaN matched NaN!")
    case _ => println("NaN matching was good")
  }

  var z = 0.0f
  var i = 0
  while (i < 10) {
    if (i % 2 == 0) z = NaN
    else z = NaN
    i += 1
  }
  if (z.isNaN && i == 10) println("loop with NaN was good")
  else println("loop with NaN was broken")
}
