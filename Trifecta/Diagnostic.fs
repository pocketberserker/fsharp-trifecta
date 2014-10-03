namespace Trifecta

open FSharp.Karma

[<AbstractClass>]
type Diagnostic<'T>() =
  abstract member Fail: Document -> _1<'T, 'U>
  abstract member Empty: unit -> _1<'T, 'U>
  abstract member Raise: Pos * Document * Document list -> _1<'T, 'U>
  default this.Raise(p: Pos, d: Document, aux: Document list) =
    this.Fail(p.Report(d, Array.ofList aux))

module Diagnostic =

  let inline fail (d: #Diagnostic<_>) doc = d.Fail(doc)

  let failUnless (m: #Monad<_>) (d: #Diagnostic<_>) b e =
    if b then Applicative.point m (fun () -> ()) else fail d e

  let failWhen (m: #Monad<_>) (d: #Diagnostic<_>) b e =
    if b then fail d e else Applicative.point m (fun () -> ())

  let inline raise (d: #Diagnostic<_>) p doc extra = d.Raise(p, doc, extra)

  let raiseUnless (m: #Monad<_>) (d: #Diagnostic<_>) b p e xs =
    if b then Applicative.point m (fun () -> ()) else raise d p e xs

  let raiseWhen (m: #Monad<_>) (d: #Diagnostic<_>) b p e xs =
    if b then raise d p e xs else Applicative.point m (fun () -> ())

  let inline empty (d: #Diagnostic<_>) = d.Empty()

  let guard (m: #Monad<_>) (d: #Diagnostic<_>) b =
    if b then Applicative.point m (fun () -> ()) else empty d
