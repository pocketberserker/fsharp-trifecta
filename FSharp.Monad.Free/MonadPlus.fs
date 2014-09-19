namespace FSharp.Monad

[<AbstractClass>]
type MonadPlus<'F>() =
  inherit Monad<'F>()
  abstract member Empty: unit -> #_1<'F, 'A>
  abstract member Plus: #_1<'F, 'A> * #_1<'F, 'A> -> #_1<'F, 'A>
  interface PlusEmpty<'F> with
    member this.Plus(f1, f2) = this.Plus(f1, f2)
    member this.Empty() = this.Empty()
