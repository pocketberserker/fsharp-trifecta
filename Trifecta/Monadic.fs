namespace Trifecta

open FSharp.Monad

type Scoped<'T, 'A> =
  abstract member Self: _1<'T, 'A>
  abstract member Scope: string -> _1<'T, 'A>

type Functorial<'T, 'A> =
  abstract member Self: unit -> _1<'T, 'A>
  abstract member Map: ('A -> 'B) -> _1<'T, 'B>
  abstract member As: 'B -> _1<'T, 'B>
  //member this.As(b) = this.Map(fun _ -> b)
  abstract member Skip: unit -> _1<'T, unit>
  //member this.Skip() = this.As(())

// TODO: implement collect
type Filtered<'T, 'A> =
  inherit Functorial<'T, 'A>
  abstract member LiftF: _1<'T ,'B> -> Filtered<'T, 'B>
  abstract member WithFilter: ('A -> bool) -> _1<'T, 'A>
  abstract member FilterMap: ('A -> 'B option) -> _1<'T, 'B>
  abstract member Filter: ('A -> bool) -> _1<'T, 'A>
  //member this.FilterMap(f: 'A -> _ option) =
  //  this.LiftF(this.LiftF(this.Map(f)).WithFilter(Option.isSome)).Map(Option.get)
  //member this.Filter(p) = this.WithFilter(p)

type AppliedOnce<'T, 'A> =
  inherit Functorial<'T, 'A>
  abstract member Map2: _1<'T, 'B> * ('A * 'B -> 'C) -> _1<'T, 'C>

module AppliedOnce =
  let (++) (a: #AppliedOnce<'T, 'A>) (m: #_1<'T, 'B>) = a.Map2(m, id)
  let (>>.) (a: #AppliedOnce<'T, 'A>) (m: #_1<'T, 'B>) = a.Map2(m, snd)
  let (.>>) (a: #AppliedOnce<'T, 'A>) (m: #_1<'T, _>) = a.Map2(m, fst)

open AppliedOnce

type Applied<'T, 'A> =
  inherit AppliedOnce<'T, 'A>
  abstract member LiftAp: #_1<'T ,'B> -> #Applied<'T, 'B>

module Applied =

  let between (a: #Applied<'T, 'A>) (bra: #_1<'T, _>) (ket: #_1<'T, _>) =
    a.LiftAp(a .>> ket) >>. bra

[<AbstractClass>]
type Alternating<'T, 'A>() =
  abstract member Self: unit -> _1<'T, 'A>
  abstract member Map: ('A -> 'B) -> _1<'T, 'B>
  member this.As(b) = this.Map(fun _ -> b)
  member this.Skip() = this.As(())
  abstract member Map2: _1<'T, 'B> * ('A * 'B -> 'C) -> _1<'T, 'C>
  abstract member LiftAl: _1<'T, 'B> -> Alternating<'T, 'B>
  abstract member Or: _1<'T, 'A> -> _1<'T, 'A>
  abstract member OrElse: 'A -> _1<'T, 'A>
  member this.Optiona = this.LiftAl(this.Map(Some)).OrElse(None)
  member this.SkipOptional = this.LiftAl(this.Skip()).OrElse(())
  member this.Many() = this.LiftAl(this.Some()).OrElse([])
  member this.Some() = this.Map2(this.Many(), fun (a, b) -> a :: b)
  member this.SkipMany() = this.LiftAl(this.SkipSome()).OrElse(())
  member this.SkipSome() = this >>. this.SkipMany()
  member this.SepBy(sep) = this.LiftAl(this.SepBy1(sep)).OrElse([])
  member this.SepBy1(sep) =
    this.Map2(this.LiftAl(this.LiftAl(sep) >>. this.Self()).Many(), fun (a, b) -> a :: b)
  member this.SkipSepBy(sep) = this.LiftAl(this.SkipSepBy1(sep)).OrElse(())
  member this.SkipSepBy1(sep) = this.LiftAl(this.LiftAl(sep) >>. this.Self()).SkipMany()
  member this.Chainr(op: _1<'T, 'A * 'A -> 'A>) =
    this.Map2(this.LiftAl(this.LiftAl(op).Map2(this.Chainr(op), fun (f, x) -> fun y -> f (y, x))).OrElse(id), fun (b, f) -> f b)
  member this.Chainl(op: _1<'T, 'A * 'A -> 'A>) =
    let rec rest = lazy begin
      this.LiftAl(this.LiftAl(this.LiftAl(op).Map2(this.Self(), fun (f, x) -> fun y -> f (y, x))).Map2(rest.Force(), fun (fp, g) -> g >> fp)).OrElse(id)
    end
    this.Map2(rest.Force(), fun (b, f) -> f b)
  member this.ManyTill(e) =
    this.LiftAl(this.LiftAl(e).As([])).Or(this.Map2(this.ManyTill(e), fun (a, b) -> a :: b))
  member this.SkipManyTill(e) =
    this.LiftAl(this.LiftAl(e).Skip()).Or(this >>. this.SkipManyTill(e))
  member this.EndBy1(sep) = this.LiftAl(this .>> sep).Some
  member this.endBy(sep) = this.LiftAl(this .>> sep).Many
  member this.SkipEndBy1(sep) = this.LiftAl(this .>> sep).SkipSome
  member this.SkipEndBy(sep) = this.LiftAl(this .>> sep).SkipMany
  member this.SepEndBy1(sep) =
    this.Map2(this.LiftAl(this.LiftAl(this.LiftAl(sep) >>. this.SepEndBy(sep)).Map(fun as_ -> fun a -> a :: as_)).OrElse(fun a -> [a]), fun (b, f) -> f b)
  member this.SepEndBy(sep) = this.LiftAl(this.SepEndBy1(sep)).OrElse([])
  member this.SkipSepEndBy1(sep) =
    this >>. (this.LiftAl(this.LiftAl(sep) >>. this.SkipSepEndBy(sep)).OrElse(()))
  member this.SkipSepEndBy(sep) = this.LiftAl(this.SkipSepEndBy1(sep)).OrElse(())
  abstract member LiftF: _1<'T ,'B> -> Filtered<'T, 'B>
  abstract member WithFilter: ('A -> bool) -> _1<'T, 'A>
  member this.FilterMap(f: 'A -> _ option) =
    this.LiftF(this.LiftF(this.Map(f)).WithFilter(Option.isSome)).Map(Option.get)
  member this.Filter(p) = this.WithFilter(p)
  interface Functorial<'T, 'A> with
    member this.Self() = this.Self()
    member this.Map(f) = this.Map(f)
    member this.As(b) = this.As(b)
    member this.Skip() = this.Skip()
  interface Filtered<'T, 'A> with
    member this.LiftF(v) = this.LiftF(v)
    member this.WithFilter(p) = this.WithFilter(p)
    member this.FilterMap(f) = this.FilterMap(f)
    member this.Filter(p) = this.Filter(p)
  interface Applied<'T, 'A> with
    member this.LiftAp(v) = (this :> Applied<_, _>).LiftAp(v)
    member this.Map2(v, f) = this.Map2(v, f)

type Monadic<'T, 'A> =
  inherit Applied<'T, 'A>
  abstract member LiftM: _1<'T, 'B> -> Monadic<'T, 'B>
  abstract member Bind: ('A -> _1<'T, 'B>) -> _1<'T, 'B>
  //override this.Map2(m, f) = this.Bind(fun a -> this.LiftM(m).Map(fun b -> f (a, b)))
  abstract member When: bool -> _1<'T, unit>
  abstract member Unless: bool -> _1<'T, unit>
  //member this.Unless(b) = this.When(not b)

module Monadic =
  let (>>.) (M: #Monadic<'T, _>) (m: _1<'T, 'B>) = M.Bind(fun _ -> m)
  let (.>>) (M: #Monadic<'T, _>) (m: #_1<'T, _>) = M.Bind(fun a -> M.LiftM(m).As(a))

// TODO: implement flatMatch
[<AbstractClass>]
type MonadicPlus<'T, 'A>() =
  inherit Alternating<'T, 'A>()
  abstract member LiftM: _1<'T ,'B> -> Monadic<'T, 'B>
  abstract member LiftMp: _1<'T ,'B> -> MonadicPlus<'T, 'B>
  abstract member Bind: ('A -> _1<'T, 'B>) -> _1<'T, 'B>
  abstract member When: bool -> _1<'T, unit>
  override this.Map2(m, f) = this.Bind(fun a -> this.LiftMp(m).Map(fun b -> f (a, b)))
  member this.Unless(b) = this.When(not b)
  member this.FlatMatch(f) = this.LiftMp(this.WithFilter(f >> Option.isSome)).Bind(f >> Option.get)
  interface Monadic<'T, 'A> with
    member this.LiftM(v) = this.LiftM(v)
    member this.Bind(f) = this.Bind(f)
    member this.When(b) = this.When(b)
    member this.Unless(b) = this.Unless(b)

type Comonadic<'T, 'A> =
  inherit Functorial<'T, 'A>
  abstract member LiftC: _1<'T, 'B> -> Comonadic<'T, 'B>
  //override this.Map(f) = this.Extend(fun ta -> f (this.LiftC(ta).Extract))
  abstract member Map: ('A -> 'B) -> _1<'T, 'B>
  abstract member Extend: (_1<'T, 'A> -> 'B) -> _1<'T, 'B>
  abstract member Extract: 'A
