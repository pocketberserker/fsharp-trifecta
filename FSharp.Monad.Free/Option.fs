namespace FSharp.Monad

module Option =

  let monoid (s: Semigroup<_>) = { new Monoid<_> with
    member this.Append(f1, f2) =
      match (f1, f2) with
      | Some a1, Some a2 -> Some (s.Append(a1, a2))
      | Some _, None -> f1
      | None, Some _ -> f2
      | None, None -> None
    member this.Zero() = None }

  let getOrElse defaultValue = function
    | Some v -> v
    | None -> defaultValue

  let orElse d = function
    | Some _ as v -> v
    | None -> d ()
