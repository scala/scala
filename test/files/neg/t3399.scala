object Nats {
    sealed trait Nat {
        // fold right on N, N-1, ..., 1
        type FoldR[Init <: Type, Type, F <: Fold[Nat, Type]] <: Type
    }
    sealed trait _0 extends Nat {
        type FoldR[Init <: Type, Type, F <: Fold[Nat, Type]] = Init
    }
    sealed trait Succ[N <: Nat] extends Nat {
        type FoldR[Init <: Type, Type, F <: Fold[Nat, Type]] =
          F#Apply[Succ[N], N#FoldR[Init, Type, F]]
    }

    type Add[A <: Nat, B <: Nat] = A#FoldR[B, Nat, Inc]
    trait Fold[-Elem, Value] {
        type Apply[N <: Elem, Acc <: Value] <: Value
    }
    type Inc = Fold[Any, Nat] {
        type Apply[N <: Any, Acc <: Nat] = Succ[Acc]
    }

    type _1 = Succ[_0]
    implicitly[ Add[_1, _1] =:= _1]
}