package p {                   // `X' bound by package clause
import Console._              // `println' bound by wildcard import
object Y {
  println(s"L4: $X")          // `X' refers to `p.X' here
  locally {
    import q._                // `X' bound by wildcard import
    println(s"L7: $X")        // `X' refers to `q.X' here
    import X._                // `x' and `y' bound by wildcard import
    println(s"L9: $x")        // `x' refers to `q.X.x' here
    locally {
      val x = 3               // `x' bound by local definition
      println(s"L12: $x")     // `x' refers to constant `3' here
      locally {
        import q.X._          // `x' and `y' bound by wildcard import
        println(s"L15: $x")   // reference to `x' is ambiguous here
        import X.y            // `y' bound by explicit import
        println(s"L17: $y")   // `y' refers to `q.X.y' here
        locally {
          val x = "abc"       // `x' bound by local definition
          import p.X._        // `x' and `y' bound by wildcard import
          println(s"L21: $y") // reference to `y' is ambiguous here
          println(s"L22: $x") // `x' refers to string "abc" here
}}}}}}

