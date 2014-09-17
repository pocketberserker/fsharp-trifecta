namespace Trifecta

open FSharp.Monad

type Result = Result

type Result<'S, 'A> =
  | Success of body: 'A * state: 'S
  | Failure of error: Document option * stack: string list
  with
    interface _1<Result, 'A>
    interface Functorial<Result, 'A> with
      member this.Map(f) =
        match this with
        | Success(body, state) -> Success(f body, state)
        | Failure(e, s) -> Failure(e, s)
        :> _1<Result, _>
      member this.Self() = this :> _1<_, _>
      member this.As(b) =
        match this with
        | Success _ -> (this :> Functorial<Result, _>).Map(fun _ -> b)
        | Failure(e, s) -> Failure(e, s) :> _1<_, _>
      member this.Skip() = (this :> Functorial<Result, _>).As(())

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Result =

  let map f (r: Result<_, _>) = (r :> Functorial<_, _>).Map(f)
  let self (r: Result<_, _>) = (r :> Functorial<_, _>).Self()
  let as_ b (r: Result<_, _>) = (r :> Functorial<_, _>).As(b)
  let skip (r: Result<_, _>) = (r :> Functorial<_, _>).Skip()
