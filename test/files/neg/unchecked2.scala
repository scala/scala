object Test {
  // These warn because it can be statically shown they won't match.

  /*   warn */ Some(List(1)).isInstanceOf[Option[List[String]]]
  /*   warn */ Some(123).isInstanceOf[Option[Option[_]]]
  /*   warn */ Some(123).isInstanceOf[Option[String]]
  /*   warn */ Some(123).isInstanceOf[Option[List[String]]]
  /*   warn */ Some(123).isInstanceOf[Option[List[Int => String]]]
  /*   warn */ Some(123).isInstanceOf[Option[(String, Double)]]
  /*   warn */ Some(123).isInstanceOf[Option[String => Double]]

  // These warn because you can't check at runtime.

  /*   warn */ (Some(List(1)): Any).isInstanceOf[Option[List[String]]]
  /*   warn */ (Some(123): Any).isInstanceOf[Option[Int]]
  /*   warn */ (Some(123): Any).isInstanceOf[Option[String]]
  /*   warn */ (Some(123): Any).isInstanceOf[Option[List[String]]]
  /*   warn */ (Some(123): Any).isInstanceOf[Option[List[Int => String]]]
  /*   warn */ (Some(123): Any).isInstanceOf[Option[(String, Double)]]
  /*   warn */ (Some(123): Any).isInstanceOf[Option[String => Double]]

  // These don't warn.

  /* nowarn */ Some(List(1)).isInstanceOf[Option[List[Int]]]
  /* nowarn */ Some(123).isInstanceOf[Option[Int]]
  /* nowarn */ Some(123).isInstanceOf[Some[Int]]
  /* nowarn */ Some(123).isInstanceOf[AnyRef]

  /* nowarn */ (Some(List(1)): Any).isInstanceOf[Option[_]]
  /* nowarn */ (Some(123): Any).isInstanceOf[Option[_]]
  /* nowarn */ (Some(123): Any).isInstanceOf[Some[_]]
  /* nowarn */ (Some(123): Any).isInstanceOf[AnyRef]
}
