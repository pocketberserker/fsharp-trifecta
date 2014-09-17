namespace FSharp.Monad

type Monoid<'F> =
  inherit Semigroup<'F>
  abstract member Zero: unit -> 'F

module Monoid =

  let zero (m: #Monoid<_>) = m.Zero()
  let append (m: #Monoid<_>) a b = m.Append(a, b)

  let instance f z = { new Monoid<_> with
    member this.Zero() = z
    member this.Append(f1, f2) = f f1 f2 }
