namespace FSharp.Monad

type Ordering =
  | LT
  | EQ
  | GT

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Ordering =

  let toInt = function
    | LT -> -1
    | EQ -> 0
    | GT -> 1

  let toString = function
    | LT -> "LT"
    | EQ -> "EQ"
    | GT -> "GT"

  let nameValuePair o = (toInt o, toString o)

  let complement = function
    | LT -> GT
    | EQ -> EQ
    | GT -> LT

  let monoid = { new Monoid<Ordering> with
    member this.Append(f1, f2) =
      match f1 with
      | EQ -> f2
      | _ -> f1
    member this.Zero() = EQ }

  let fromLessThan f a1 a2 =
    if f a1 a2 then LT
    elif f a2 a1 then GT
    else EQ

  let fromInt intOrdering = if intOrdering < 0 then LT elif intOrdering > 0 then GT else EQ
