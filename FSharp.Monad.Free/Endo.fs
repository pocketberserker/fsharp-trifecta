namespace FSharp.Monad

type Endo<'T> = Endo of ('T -> 'T)

module Endo =

  let apply a (Endo run) = run a

  let compose (Endo other) (Endo run) = Endo(run >> other)

  let monoid = { new Monoid<_> with
    member this.Append(f1, f2) = f1 |> compose f2
    member this.Zero() = Endo(id) }
