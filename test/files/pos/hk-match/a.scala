trait A {
  type HKAlias[X] = List[X]

  (null: Any) match { case f: Bippy[HKAlias] => f }
}
