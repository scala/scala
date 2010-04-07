/** Test case for partially specialized classes. see #2880. */

class Arc[State, @specialized T](label: T, to: State)


