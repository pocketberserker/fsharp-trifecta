namespace Trifecta

open FSharp.Monad

type ParseResult = ParseResult
type ParseResult<'S, 'A> =
  inherit Functorial<ParseResult, 'A>
  inherit _1<ParseResult, 'A>

type ParseFailure<'S, 'A> =
  inherit ParseResult<'S, 'A>
  abstract member Cast: unit -> ParseFailure<'S, 'B>

[<Sealed>]
type Fail<'S, 'A>(msg: Document option, aux: Document list, expected: Set<string>) =
  member this.Message = msg
  member this.Aux = aux
  member this.Expected = expected
  member this.Map(_) = Fail(msg, aux, expected) :> _1<_, _>
  member this.As(b) = this.Map(fun _ -> b)
  member this.Self = this :> _1<_, _>
  member this.Skip = this.As(())
  member this.Append(m: Fail<_, _>) =
    let msg =  m.Message |> Option.orElse (fun () -> msg)
    let aux =
      match m.Message, msg with
      | (Some _, _) -> m.Aux
      | (_, Some _) -> aux
      | _ -> List.append m.Aux aux
    Fail(msg, aux, m.Expected + expected)
  interface ParseFailure<'S, 'A> with
    member this.Map(f) = this.Map(f)
    member this.As(b) = this.As(b)
    member this.Self() = this.Self
    member this.Skip() = this.Skip
    member this.Cast() = Fail(msg, aux, expected) :> ParseFailure<_, _>

[<AutoOpen>]
module FailOps =

  let (++) (a1: Fail<_, _>) (a2: Fail<_, _>) = a1.Append(a2)

[<Sealed>]
type Error<'S, 'A>(loc: Pos, msg: Document, aux: Document list, stack: (Pos * string) list) =
  inherit Located()
  member this.Msg = msg
  member this.Aux = aux
  member this.Stack = stack
  override this.Loc = loc :> Loc
  member this.Pos = loc
  member this.Map(_) = Error(loc, msg, aux, stack) :> _1<_, _>
  member this.As(b) = this.Map(fun _ -> b)
  member this.Self = this :> _1<_, _>
  member this.Skip = this.As(())
  member this.Pretty = this.Report(msg, Array.ofList aux)
  override this.ToString() = this.Pretty |> Document.toString
  interface ParseFailure<'S, 'A> with
    member this.Map(f) = this.Map(f)
    member this.As(b) = this.As(b)
    member this.Self() = this.Self
    member this.Skip() = this.Skip
    member this.Cast() = Error(loc, msg, aux, stack) :> ParseFailure<_, _>

module Error =

  let apply loc msg aux stack = Error(loc, msg, aux, stack)
  let unapply (e: Error<_, _>) = Some(e.Loc, e.Msg, e.Aux, e.Stack)
  let report loc msg aux expected =
    let msg =
      match Set.isEmpty expected with
      | true -> msg |> Option.getOrElse (DocText "syntax error")
      | false ->
        let expl = DocText "expected" <+> Document.nest 4 (Document.oxford (DocText "or") (expected |> Set.toList |> List.sort |> List.map (fun x -> DocText x)))
        match msg with
        | Some e -> (e @@ ",") @//@ expl
        | None -> expl
    Error(loc, msg, aux, [])

type Pure<'S, 'A>(extract: 'A, last: Fail<'S, 'A>) =
  let fail (f: Fail<_, _>) = Fail(f.Message, f.Aux, f.Expected)
  member this.Last = last
  member this.Map(f) = Pure(f extract, fail last) :> _1<_, _>
  member this.As(b) = Pure(b, fail last) :> _1<_, _>
  member this.Self = this :> _1<_, _>
  member this.Skip = this.As(())
  member this.Extend(f) = Pure(this :> _1<_, _> |> f, fail last) :> _1<_, _>
  member this.Extract = extract
  interface Functorial<ParseResult, 'A> with
    member this.Map(f) = this.Map(f)
    member this.As(b) = this.As(b)
    member this.Self() = this.Self
    member this.Skip() = this.Skip
  interface ParseResult<'S, 'A>
  interface Comonadic<ParseResult, 'A> with
    member this.Extend(f) = this.Extend(f)
    member this.LiftC(v) = v :?> Pure<_, _> :> Comonadic<ParseResult, _>
    member this.Map(f) = this.Map(f)
    member this.Extract = this.Extract

module Pure =

  let extend f (p: #Pure<_, _>) = p.Extend(f)

type Parser = Parser

// FIXME: type parameter of endsWith
type LayoutContext<'S> =
  | IndentedLayout of depth: int * desc: string
  | BracedLayout of left: string * endsWith: Parser<'S, obj> * unmatchedBy: Parser<'S, obj> * right: string
  with
    override this.ToString() =
      match this with
      | IndentedLayout(depth, desc) -> "indented " + desc + " (" + string depth + ")"
      | BracedLayout(l, _, _, r) -> l + " " + r

and ParseState<'S>(loc: Pos, input: string, offset: int, s: 'S, layoutStack: LayoutContext<'S> list, bol: bool) =
  inherit Located()
  member this.Depth =
    match layoutStack with
    | IndentedLayout(n, _)::_ -> n
    | (BracedLayout _)::_ -> 0
    | [] -> 0
  member this.Pos = loc
  member this.Input = input
  member this.Offset = offset
  member this.S = s
  member this.LayoutStack = layoutStack
  member this.Bol = bol
  member this.Tracing() = true
  override this.Loc = loc :> Loc

and Commit<'S, 'A>(s: ParseState<'S>, extract: 'A, expected: Set<string>) =
  inherit Located()
  member this.S = s
  member this.Expected = expected
  override this.Loc = s.Loc
  member this.Pos = s.Pos
  member this.Map(f) = Commit(s, f extract, expected) :> _1<_, _>
  member this.As(b) = Commit<_, _>(s, b, expected) :> _1<_, _>
  member this.Self = this :> _1<_, _>
  member this.Skip = this.As(())
  member this.Extend(f) = Commit(s, this :> _1<_, _> |> f, expected) :> _1<_, _>
  member this.Extract = extract
  interface Functorial<ParseResult, 'A> with
    member this.Map(f) = this.Map(f)
    member this.As(b) = this.As(b)
    member this.Self() = this.Self
    member this.Skip() = this.Skip
  interface ParseResult<'S, 'A>
  interface Comonadic<ParseResult, 'A> with
    member this.Extend(f) = this.Extend(f)
    member this.LiftC(v) = v :?> Commit<_, _> :> Comonadic<ParseResult, _>
    member this.Map(f) = this.Map(f)
    member this.Extract = this.Extract

and [<AbstractClass>] Parser<'S, 'A>() =
  inherit MonadicPlus<Parser, 'A>()
  abstract member Apply: ParseState<'S> * Supply -> Trampoline<ParseResult<'S, 'A>>
  override this.Self() = this :> _1<_, _>
  override this.Map(f) =
    { new Parser<_, _>() with
      member x.Apply(s, vs) = this.Apply(s, vs) |> Free.map (fun x -> x.Map(f) :?> ParseResult<_, _>) }
    :> _1<_, _>
  override this.Bind(f) =
    { new Parser<_, _>() with
      member x.Apply(s, vs) =
        this.Apply(s, vs) |> Free.bind (function
          | :? Pure<'S, 'A> as r -> (f r.Extract :?> Parser<_, _>).Apply(s, vs) |> Free.map (function
            | :? Pure<'S, _> as p -> Pure(p.Extract, r.Last ++ p.Last) :> ParseResult<_, _>
            | :? Fail<'S, _> as f -> r.Last ++ f :> ParseResult<_, _>
            | r -> r)
          | :? Commit<'S, 'A> as c ->
            (f c.Extract :?> Parser<_, _>).Apply(c.S, vs) |> Free.map (function
              | :? Pure<'S, _> as p ->
                Commit(c.S, p.Extract, Set.union c.Expected p.Last.Expected)
                :> ParseResult<_, _>
              | :? Fail<'S, _> as f ->
                Error.report c.Pos f.Message  f.Aux (Set.union c.Expected f.Expected)
                :> ParseResult<_, _>
              | r -> r)
          | r -> Trampoline.delay (fun () -> (r :?> ParseFailure<_, _>).Cast() :> ParseResult<_, _>)) }
    :> _1<_, _>
  override this.WithFilter(pred) =
    { new Parser<_, _>() with
      member x.Apply(s, vs) =
        this.Apply(s, vs) |> Free.map (function
          | :? Pure<'S, 'A> as p when not <| pred p.Extract -> p.Last :> ParseResult<_, _>
          | :? Commit<'S, 'A> as c when not <| pred c.Extract ->
            Error.report c.Pos None [] c.Expected :> ParseResult<_, _>
          | r -> r) }
    :> _1<_, _>
  override this.Or(other) =
    { new Parser<_, 'A>() with
      member x.Apply(s, vs) =
        this.Apply(s, vs) |> Free.bind (function
          | :? Fail<'S, 'A> as f ->
            (other :?> Parser<_, _>).Apply(s, vs) |> Free.map (function
              | :? Fail<'S, 'A> as e -> f ++ e :> ParseResult<_,_>
              | :? Pure<'S, 'A> as p-> Pure(p.Extract, f ++ p.Last) :> ParseResult<_, _>
              | r -> r)
          | r -> Trampoline.delay (fun () -> r)) }
    :> _1<_, _>
  override this.OrElse(a) =
    { new Parser<_, _>() with
      member x.Apply(s, vs) =
        this.Apply(s, vs) |> Free.map (function
          | :? Fail<'S, 'A> as f -> Pure(a, f) :> ParseResult<_, _>
          | r -> r) }
    :> _1<_, _>
  member this.ApplyF(f: ParseState<_> -> Supply -> ParseResult<_, _>) =
    { new Parser<_, _>() with
      member x.Apply(s, vs) = Trampoline.delay (fun () -> f s vs) }
  override this.When(b) =
    if b then this.Skip()
    else this.ApplyF(fun _ _ -> Pure((), Fail(None, [], Set.empty)) :> ParseResult<_, _>) :> _1<_, _>
  override this.LiftF(f) = f :?> Parser<_, _> :> Filtered<_, _>
  override this.LiftM(m) = m :?> Parser<_, _> :> Monadic<_, _>
  override this.LiftAl(a) = a :?> Parser<_, _> :> Alternating<_, _>
  override this.LiftMp(m) = m :?> Parser<_, _> :> MonadicPlus<_, _>
  member this.Race(p: Parser<_, _>) =
    { new Parser<'S, 'A>() with
      member x.Apply(s, vs) =
        this.Apply(s, vs) |> Free.bind (function
          | :? Fail<'S, 'A> as f ->
            p.Apply(s, vs) |> Free.map (function
              | :? Fail<'S, 'A> as ep -> f ++ ep :> ParseResult<_, _>
              | :? Pure<'S, 'A> as r -> Pure(r.Extract, f ++ r.Last) :> ParseResult<_, _>
              | r -> r)
          | :? Error<'S, 'A> as e ->
            p.Apply(s, vs) |> Free.map (function
              | :? Fail<'S, 'A> -> e :> ParseResult<_, _>
              | :? Error<'S, 'A> as er ->
                match Pos.order.Order(e.Pos, er.Pos) with
                | LT -> er
                | EQ -> e
                | GT -> e
                :> ParseResult<_, _>
              | r -> r)
          | r -> Trampoline.delay (fun () -> r)) }
  member this.Scope(desc) =
    { new Parser<_, _>() with
      member x.Apply(s, vs) =
        this.Apply(s, vs) |> Free.map (function
          | :? Fail<'S, 'A> as f -> Fail(f.Message, f.Aux, Set.empty |> Set.add desc) :> ParseResult<_, _>
          | :? Error<'S, 'A> as e when s.Tracing() ->
            Error(e.Pos, e.Msg, e.Aux, (e.Pos, desc) :: e.Stack) :> ParseResult<_, _>
          | :? Pure<'S, 'A> as p ->
            Pure(p.Extract, Fail(p.Last.Message, p.Last.Aux, Set.empty |> Set.add desc)) :> ParseResult<_, _>
          | r -> r) }
  member this.Attempt() =
    { new Parser<_, _>() with
      member x.Apply(s, vs) =
        this.Apply(s, vs) |> Free.map (function
          | :? Error<'S, 'A> as e -> Fail(None, [e.Pretty], Set.empty) :> ParseResult<_, _>
          | r -> r) }
  member this.Not() =
    { new Parser<_, _>() with
      member x.Apply(s, vs) =
        this.Apply(s, vs) |> Free.map (function
          | :? Pure<'S, 'A> as p -> Fail(Some((DocText "") <+> (DocText(p.Extract.ToString()))), [], Set.empty) :> ParseResult<_, _>
          | :? Commit<'S, 'A> as c -> Error.report c.Pos (Some((DocText "") <+> (DocText(c.Extract.ToString())))) [] Set.empty :> ParseResult<_, _>
          | _ -> Pure((), Fail(None, [], Set.empty)) :> ParseResult<_, _>) }
  member this.Slice() =
    { new Parser<_, _>() with
      member x.Apply(s, vs) =
        this.Apply(s, vs) |> Free.map (function
          | :? Pure<'S, 'A> as p -> Pure("", Fail(p.Last.Message, p.Last.Aux, p.Last.Expected)) :> ParseResult<_, _>
          | :? Commit<'S, 'A> as c -> Commit(c.S, s.Input.Substring(s.Offset, c.S.Offset), c.Expected) :> ParseResult<_, _>
          | r -> (r :?> ParseFailure<'S, 'A>).Cast() :> ParseResult<_, _>) }
  member this.Handle(f: ParseFailure<_, _> -> Parser<_, _>) =
    { new Parser<_, _>() with
      member x.Apply(s, vs) =
        this.Apply(s, vs) |> Free.bind (function
          | :? Error<'S, 'A> as e -> (f e).Apply(s, vs)
          | :? Fail<'S, 'A> as r ->
            (f r).Apply(s, vs) |> Free.map (function
              | :? Fail<'S, 'A> as e ->
                let msg = e.Message |> Option.orElse (fun () -> r.Message)
                let aux = if e.Message |> Option.isSome then e.Aux else r.Aux
                Fail(msg, aux, Set.union r.Expected e.Expected) :> ParseResult<_, _>
              | rr -> rr)
          | r -> Trampoline.delay (fun () -> r)) }
  interface _1<Parser, 'A>

module ParseState =

  let mk fileName content initialState =
    ParseState(Pos.start fileName content, content, 0, initialState, [IndentedLayout(1, "toplevel")], false)

  let tracing (s: ParseState<_>) = s.Tracing()

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Parser =

  let apply f = { new Parser<_, _>() with
    member this.Apply(s, vs) = Trampoline.delay (fun () -> f s vs) }

  let run s vs (p: Parser<_, _>) =
    match p.Apply(s, vs) |> Free.run F0.functor_ Free.castF0 with
    | :? Pure<'S, _> as p -> Choice1Of2(s, p.Extract)
    | :? Commit<'S, _> as c -> Choice1Of2(c.S, c.Extract)
    | :? Fail<'S, _> as f -> Choice2Of2(Error.report s.Pos f.Message f.Aux f.Expected)
    | _ as e -> Choice2Of2(e :?> Error<_, _>)

  let inline bind (f: 'A -> Parser<'F, 'B>) (p: Parser<'F, 'A>) = p.Bind(f >> (fun x -> x :> _1<Parser, 'B>)) :?> Parser<'F, 'B>
  let inline (>>=) p f = bind f p

  let map (f: 'A -> 'B) (p: Parser<'F, 'A>) = p.Map(f) :?> Parser<'F, 'B>
  let inline (|>>) p f = map f p

  let inline (<|>) (p1: Parser<'F, 'A>) (p2: Parser<'F, 'A>) = p1.Or(p2 :> _1<Parser, 'A>) :?> Parser<'F, 'A>

  let wouldSucceed<'S> = { new Parser<'S, bool>() with
    member this.Apply(s, vs) =
      this.Apply(s, vs) |> Free.map (function
        | :? ParseFailure<'S, bool> -> Pure(false, Fail(None, [], Set.empty)) :> ParseResult<_, _>
        | _ -> Pure(true, Fail(None, [], Set.empty)) :> ParseResult<_, _>) }

  let inline race q (p: Parser<_, _>) = p.Race(q)
  let inline scope desc (p: Parser<_, _>) = p.Scope(desc)
  let inline attempt (p: Parser<_, _>) = p.Attempt()
  let attemptScope s (p: Parser<_, _>) = p.Attempt().Scope(s)
  let inline pnot (p: Parser<_, _>) = p.Not()
  let inline slice (p: Parser<_, _>) = p.Slice()

  let monad = { new Monad<Parser>() with
    member this.Point(f) =
      { new Parser<_, _>() with
        member this.Apply(_, _) =
          Trampoline.delay (fun () -> Pure(f.Apply(), Fail(None, [], Set.empty)) :> ParseResult<_, _>)
        }
      :> _1<Parser, _>
    override this.Map(f, m) = map f (m :?> Parser<_, _>) :> _1<_, _>
    member this.Bind(StdF1 f, m) = bind (f >> fun x -> x :?> Parser<_, _>) (m :?> Parser<_, _>) :> _1<_, _> }

  type private UnitP<'F, 'A>(a: 'A) =
    inherit Parser<'F, 'A>()
    override this.Apply(s, vs) = Trampoline.delay (fun () -> Pure(a, Fail(None, [], Set.empty)) :> ParseResult<_, _>)
    override this.Map(f) = UnitP(f a) :> _1<Parser, _>
    override this.Bind(f) = f a

  let unit a = UnitP(a) :> Parser<_, _>

  type ParserBuilder () =
    member this.Return(x) = unit x
    member this.ReturnFrom(x: Parser<_, _>) = x
    member this.Bind(x, f) = x >>= f

  let parser = ParserBuilder()

  let diagnostic = { new Diagnostic<Parser>() with
    member this.Empty() =
      apply (fun _ _ -> Fail(None, [], Set.empty) :> ParseResult<_, _>) :> _1<Parser, _>
    member this.Fail(msg) =
      apply (fun _ _ -> Fail(Some msg, [], Set.empty) :> ParseResult<_, _>) :> _1<Parser, _>
    override this.Raise(p, d, aux) =
      apply (fun _ _ -> Error.report p (Some d) aux Set.empty :> ParseResult<_, _>) :> _1<Parser, _> }

  let get<'T> = apply (fun s _ -> Pure(s, Fail(None, [], Set.empty)) :> ParseResult<'T, ParseState<'T>>)
  let gets f = apply (fun s _ -> Pure(f s, Fail(None, [], Set.empty)) :> ParseResult<_, _>)
  let getSupply<'T> = apply (fun _ vs -> Pure(vs, Fail(None, [], Set.empty)) :> ParseResult<'T, _>)
  let loc<'T> = apply (fun s _ -> Pure(s.Pos, Fail(None, [], Set.empty)) :> ParseResult<'T, _>)
  let modify f = apply (fun s _ -> Commit(f s, (), Set.empty) :> ParseResult<_, _>)
  let put s = apply (fun _ _ -> Commit(s, (), Set.empty) :> ParseResult<_, _>)
  let freshId<'T> = apply (fun _ vs -> Pure(vs.Fresh(), Fail(None, [], Set.empty)) :> ParseResult<'T, _>)
  let rawSatisfy p = apply (fun s _ ->
    let si = s.Input
    if s.Offset = si.Length then Fail(None, [], Set.empty) :> ParseResult<_, _>
    else
      let so = s.Offset
      let c = si.Chars(so)
      let sop = so + 1
      if p c then Commit(ParseState(s.Pos.Bump(c, si, sop), s.Input, sop, s.S, s.LayoutStack, s.Bol), c, Set.empty) :> ParseResult<_, _>
      else Fail(None, [], Set.empty) :> ParseResult<_, _>)

  let setBol<'T> b = parser {
    let! old = gets (fun x -> x.Bol)
    return! (modify (fun (s: ParseState<'T>) -> ParseState(s.Pos, s.Input, s.Offset, s.S, s.LayoutStack, b))).When(old <> b) :?> Parser<'T, _>
  }

  let satisfy p = setBol false >>= fun () -> rawSatisfy p

  let realEOF<'T> = apply (fun s _ ->
    if s.Offset = s.Input.Length then Pure((), Fail(None, [], Set.empty)) :> ParseResult<'T, _>
    else Fail(None, [], Set.empty |> Set.add "end of input") :> ParseResult<'T, _>)

  let warn msg = apply (fun s _ -> printfn "%s" (msg.ToString()); Pure((), Fail(None, [], Set.empty)) :> ParseResult<_, _>)
  let info msg = apply (fun s _ -> printfn "%s" (msg.ToString()); Pure((), Fail(None, [], Set.empty)) :> ParseResult<_, _>)
  
  let empty<'T, 'U> = Diagnostic.empty diagnostic :?> Parser<'T, 'U>
  let choice xs = List.foldBack (<|>) xs empty
  let assert_ p = if p then unit () else empty
  let liftOption = function
    | Some a -> unit a
    | None -> empty

  let handle f (p: Parser<_, _>) = p.Handle(f)
  let notFollowedBy p = pnot p

  let guard b = Diagnostic.guard monad diagnostic b :?> Parser<_, _>

  let stillOnside<'T> : Parser<'T, unit> = parser {
    let! b = gets (fun (s: ParseState<'T>) -> not s.Bol || s.Pos.Column > s.Depth)
    return! guard b
  }

  let rawChar (c: char) = rawSatisfy ((=) c) |> scope ("'" + string c + "'")
  let pchar c = parser {
    do! stillOnside
    let! c = rawChar c
    do! setBol false
    return c
  }

  let rawNewline<'S> : Parser<'S, char> = rawSatisfy ((=) '\n') |> scope "newline"

  let rawString (s: string) =
    s.ToCharArray()
    |> Array.toList
    |> List.ofStdList
    |> Traverse.traverse List.traverse monad (pchar >> fun x -> x :> _1<_, _>)
    :?> Parser<_, _>
    |> attemptScope ("\"" + s + "\"")
    |> fun p -> p.As(s)
    :?> Parser<_, _>

  let pstring s = parser {
    do! stillOnside
    let! s = rawString s
    do! setBol false
    return s
  }

  open System

  let upper<'T> : Parser<'T, char>  = satisfy (fun x -> Char.IsUpper(x)) |> scope "uppercase letter"
  let lower<'T> : Parser<'T, char>  = satisfy (fun x -> Char.IsLower(x)) |> scope "lowercase letter"
  let letter<'T> : Parser<'T, char>  = satisfy (fun x -> Char.IsLetter(x)) |> scope "letter"
  let rawLetter<'T> : Parser<'T, char> = rawSatisfy (fun x -> Char.IsLetter(x)) |> scope "letter"
  let digit<'T> : Parser<'T, char>  = satisfy (fun x -> Char.IsDigit(x)) |> scope "digit"
  let simpleSpace<'T> : Parser<'T, char> = satisfy (fun x -> Char.IsWhiteSpace(x)) |> scope "simple space"

  let private pushContext ctx = modify (fun s -> ParseState(s.Pos, s.Input, s.Offset, s.S, ctx :: s.LayoutStack, s.Bol))

  let private popContext msg f = parser {
    let! u = get
    return!
      if not <| List.isEmpty u.LayoutStack then
        loc
        >>= fun l -> put (ParseState(u.Pos, u.Input, u.Offset, u.S, u.LayoutStack |> List.tail, u.Bol))
      else empty
  }

  let private comment<'T> : Parser<'T, unit> =
    rawString "--"
    |> attempt
    >>= fun x -> parser {
      do! (rawSatisfy ((<>) '\n')).SkipMany() :?> Parser<_, _>
      do!  rawNewline |>> ignore <|> realEOF
      return! unit ()
    }

  let rec private blockComment<'T> : Parser<'T, bool> =
    let rec restComment hadnl =
      (rawString "-}" |> attempt).As(hadnl) :?> Parser<'T, _> <|>
      (blockComment >>= restComment) <|>
      ((rawSatisfy((<>) '\n').As(hadnl) :?> Parser<'T, _> <|> (rawNewline.As(true) :?> Parser<'T, _>)) >>= restComment)
    parser {
      let! _ = rawString "{-" |> attempt
      return! restComment(false)
    }

  let private someRealWhitespace<'T> : Parser<'T, unit> =
    (rawSatisfy (fun x -> Char.IsWhiteSpace(x) && x <> '\n')).SkipSome()
    :?> Parser<_, _>

  // TODO: move ParseState
  let layoutEndsWith (s: ParseState<_>) : Parser<'S, unit> =
    let eof: Parser<'S, unit> = apply (fun s _ ->
      if s.Offset = s.Input.Length then Pure((), Fail(None, [], Set.empty)) :> ParseResult<_, _>
      else Fail(None, [], Set.empty |> Set.add "end of input") :> ParseResult<_, _>)
    s.LayoutStack
    |> List.tryPick (function | BracedLayout(_, endsWith, _, _) -> Some (endsWith |>> ignore) | _ -> None)
    |> Option.getOrElse (eof |> scope "end of top level layout")

  let private onside spaced =
    get |> bind (fun s ->
      if s.Offset = s.Input.Length then
        match s.LayoutStack with
        | IndentedLayout(n, desc) :: xs ->
          (modify (fun x -> ParseState(x.Pos, x.Input, x.Offset, x.S, xs, true))).As(VBrace)
          :?> Parser<_, _>
        | BracedLayout(_, _, missing, _) :: _ -> missing |> map unbox<Token>
        | [] -> unit Other
      else
        layoutEndsWith s >>= fun () -> wouldSucceed |> bind (fun b ->
          if b then
            match s.LayoutStack with
            | IndentedLayout(_,desc) :: xs ->
              (modify (fun x -> ParseState(x.Pos, x.Input, x.Offset, x.S, xs, true))).As(VBrace)
              :?> Parser<_, _>
            | _ ->
              if spaced then unit WhiteSpace
              else (setBol false).As(Other) :?> Parser<_, _>
          else if spaced then unit WhiteSpace
          else (setBol false).As(Other) :?> Parser<_, _>))

  let rec whiteSpace spaced side =
    comment.As(true) :?> Parser<_, _>
    <|> blockComment
    <|> (rawNewline.As(true) :?> Parser<_, _>)
    <|> (someRealWhitespace.As(false) :?> Parser<_, _>)
    |> scope "whitespace"
    |> fun p ->
      p.Many() :?> Parser<_, _> |> bind (function
        | [] when side -> offside spaced
        | [] -> onside spaced
        | xs when xs |> List.fold (||) side -> offside true
        | xs -> onside true)

  and private offside spaced =
    get
    |> bind (fun s ->
      let col = s.Pos.Column
      match s.LayoutStack with
      | IndentedLayout(n, _) :: xs ->
        match compare col n |> Ordering.fromInt with
        | LT ->
          (modify (fun x -> ParseState(x.Pos, x.Input, x.Offset, x.S, xs,  true))).As(VBrace)
          :?> Parser<_, _>
        | EQ ->
          if s.Offset <> s.Input.Length then (setBol false).As(VSemi) :?> Parser<_, _>
          else unit Other
        | GT -> onside spaced
      | _ -> onside spaced)

  let layout<'T> : Parser<'T, Token> =
    get >>= fun s -> whiteSpace false s.Bol

  let virtualLeftBrace n =
    modify (fun s ->
      ParseState(s.Pos, s.Input, s.Offset, s.S, IndentedLayout (max s.Pos.Column s.Depth, n) :: s.LayoutStack, s.Bol))

  let inline praise p doc extra = Diagnostic.raise diagnostic p doc extra :?> Parser<_, _>

  let virtualRightBrace<'T> : Parser<'T, unit> =
    get >>= (fun s ->
      layout.FlatMatch(function
        | VBrace -> Some (unit (unit ()) :> _1<_, _>)
        | VSemi ->
          let p: Parser<'T, Parser<'T, unit>> = loc >>= (fun p -> unit (praise p (DocText "panic: trailing virtual semicolon") []))
          Some (p :> _1<_, _>)
        | Other | WhiteSpace ->
          let p: Parser<'T, Parser<'T, unit>> =
            parser {
              let! sp = get
              let! b = layoutEndsWith sp >>= fun () -> wouldSucceed
              do! Diagnostic.failUnless monad diagnostic b (DocText "end of layout not found") :?> Parser<_, _>
              return!
                Diagnostic.raiseWhen monad diagnostic
                  (List.isEmpty sp.LayoutStack || match List.head sp.LayoutStack with BracedLayout _ -> true | _ -> false) sp.Pos (DocText "panic: incorrect layout context for virtual right brace") []
                :?> Parser<_, _>
                |>> fun () -> modify (fun s -> ParseState(s.Pos, s.Input, s.Offset, s.S, List.tail sp.LayoutStack, s.Bol))
            }
          Some (p :> _1<_, _>))
      :?> Parser<_, _>
      |> attemptScope (s.LayoutStack |> List.tryPick (function
        | BracedLayout(l, _, _, r) -> Some ("end of layout (between '" + l + "' and '" + r + "')")
        | _ -> None) |> Option.getOrElse "end of layout"))

  let left lp ld rp rd = parser {
    let! start = loc
    let! _ = scope ld lp
    return!
      modify (fun s ->
        ParseState(s.Pos, s.Input, s.Offset, s.S,
          BracedLayout(ld, scope rd rp |>> box, (apply (fun s _ ->
            let f = Fail(None, [start.Report(DocText ("note: unmatched " + ld), Array.empty)], Set.empty |> Set.add rd)
            f :> ParseResult<_, _>)), rd) :: s.LayoutStack, s.Bol))
  }

  let fail doc = Diagnostic.fail diagnostic (DocText doc)

  let optionalSpace<'T> : Parser<'T, unit> =
    layout.FlatMatch(function
      | WhiteSpace -> Some (unit () :> _1<_, _>)
      | Other -> Some (unit () :> _1<_, _>)
      | VSemi -> Some (fail "vsemi in optional space")
      | VBrace -> Some (fail "vbrace in optional space"))
    :?> Parser<'T, _>
    |> attemptScope "whitespace"

  let token (p: unit -> Parser<_, _>) =
    optionalSpace.SkipOptional
    :?> Parser<_, _>
    >>= p

  let leftToken ld rd = left (token (fun () -> pstring ld)) ("'" + ld + "'") (rawString rd) ("'" + rd + "'")
  let leftBrace<'T> : Parser<'T, _> = leftToken "{" "}"
  let leftCurlyBanana<'T> : Parser<'T, _> = leftToken "{|" "|}"
  let leftBracket<'T> : Parser<'T, _> = leftToken "[" "]"
  let leftBanana<'T> : Parser<'T, _> = leftToken "(|" "|)"
  let leftEnvelope<'T> : Parser<'T, _> = leftToken "[|" "|]"

  let right<'T> : Parser<'T, unit> =
    get >>= (fun s ->
      match s.LayoutStack with
      | (BracedLayout(_,p,missing,r) as b) :: xs ->
        ( p.Scope(r) >>= fun _ ->
           modify(fun s -> ParseState(s.Pos, s.Input, s.Offset, s.S, xs, s.Bol)) >>=
           fun () -> optionalSpace.SkipOptional :?> Parser<_, _>
        ) <|> (missing |>> ignore)
      | stk -> praise s.Pos (DocText ("panic: expected braced layout, but found: " + String.Join(",", stk))) [])

  let semi<'T> : Parser<'T, char> =
    layout.FlatMatch(function
      | Other -> Some (token (fun () -> pchar ';') :> _1<_, _>)
      | VSemi -> Some (unit ';' :> _1<_, _>)
      | _ -> None)
    :?> Parser<'T, _>
    |> attemptScope "semicolon"

  let eofIgnoringLayout<'T> : Parser<'T, unit> = realEOF

  let eof<'T> : Parser<'T, unit> = realEOF.Scope("eof") :?> Parser<_, _>

  let brace (p: Parser<_, _>) = Applied.between p leftBrace right :?> Parser<_, _>

  let laidout s (p: Parser<_, _>) =
    (brace ((p.Scope(s) :?> Parser<_, _>).SepBy(token (fun () -> pchar ';')) :?> Parser<_, _>) <|>
      (Applied.between ((p.Scope(s) :?> Parser<_, _>).SepBy(semi) :?> Parser<_, _>) (virtualLeftBrace s) virtualRightBrace :?> Parser<_, _>))
    |> scope ("layout(" + s + ")")

  let phrase p =
    modify (fun s -> ParseState(s.Pos, s.Input, s.Offset, s.S, [], s.Bol))
    >>= (fun () -> simpleSpace.SkipMany() :?> Parser<_, _>)
    >>= p
    >>= fun () -> eof
  let banana (p: Parser<_, _>) = Applied.between p leftBanana right :?> Parser<_, _>
  let paren (p: Parser<_, _>) = Applied.between p (leftToken "(" ")") right :?> Parser<_, _>
  let bracket (p: Parser<_, _>) = Applied.between p leftBracket right :?> Parser<_, _>
  let envelope (p: Parser<_, _>) = Applied.between p leftEnvelope right :?> Parser<_, _>
  let curlyBanana (p: Parser<_, _>) = Applied.between p leftCurlyBanana right :?> Parser<_, _>

  let private charEscMagic = "bfnrt\\\"'" |> Seq.zip "\b\f\n\r\t\\\"'" |> Map.ofSeq
  let private charEscUnmagic = charEscMagic |> Map.toSeq |> Seq.map (fun (x, y) -> (y, x)) |> Map.ofSeq

  let private charControl<'T> : Parser<'T, char> = (pchar '^' >>= fun _ -> upper) |>> (fun c -> char (int c - int 'A'))
  let private charEsc<'T> : Parser<'T, char> = choice (charEscMagic |> Map.toSeq |> Seq.map (fun (c,d) -> (pchar c).As(d) :?> Parser<'T, _>) |> Seq.toList)
  let private escapeCode<'T> : Parser<'T, char> = (charControl <|> charEsc) |> scope "escape code" // TODO: charNum, charAscii
  let private charEscape<'T> : Parser<'T, char> = pchar '\\' >>= fun _ -> escapeCode
  let private charLetter<'T> : Parser<'T, char> = satisfy (fun c -> (c <> '\'') && (c <> '\\') && (c > '\u0016'))
  let private charChar<'T> : Parser<'T, char> = scope "character literal character" (charLetter <|> charEscape)
  let private stringLetter<'T> : Parser<'T, char> = satisfy (fun c -> (c <> '"') && (c <> '\\') && (c > '\u0016'))
  let private stringEscape<'T> : Parser<'T, char option> =
    pchar '\\' >>= (fun _ ->
      ((simpleSpace.SkipSome() :?> Parser<_, _> >>= (fun _ -> pchar '\\' |> scope "end of string gap")).As(None) :?> Parser<_, _>) <|>
      ((pchar '&').As(None) :?> Parser<_, _>) <|>
      (escapeCode |>> Some))
  let private stringChar<'T> : Parser<'T, char option> = (stringLetter |>> Some) <|> stringEscape |> scope "string literal character"

  let charLiteral<'T> : Parser<'T, char> =
    token (fun () -> Applied.between charChar (pchar '\'') (pchar '\'') :?> Parser<_, _>) |> scope "character literal"

  let stringLiteral<'T> : Parser<'T, string> =
    token (fun () ->
      Applied.between (stringChar.Many() :?> Parser<'T, char option list>) (pchar '"') (pchar '"') :?> Parser<'T, char option list>
      |>> (fun s ->
        //_.sequence[Option,Char].getOrElse(List()).mkString
        System.String(List.foldBack (fun x acc -> match acc, x with | Some acc, Some x -> Some (x :: acc) | _ -> None) s None |> Option.getOrElse [] |> List.toArray))
    |> scope "string literal")

  // TODO: inverseStringLiteral

  let doubleLiteral_<'T> : Parser<'T, float> =
    (digit.SkipSome() :?> Parser<_, _>
    >>= fun () ->
      (((pchar '.' >>= fun _ -> digit.SkipMany() :?> Parser<_, _>)
        >>= fun () ->
          (pchar 'e' >>= fun _ -> digit.SkipSome() :?> Parser<_, _>).SkipOptional :?> Parser<_, _>)
      <|> ((pchar 'e' >>= fun _ -> digit.SkipSome() :?> Parser<_, _>))))
    |> attempt
    |> slice
    |>> float
  let doubleLiteral<'T> : Parser<'T, float> = token (fun () -> doubleLiteral_)

  // TODO: dateLiteral_
  // TODO: dateLiteral

  let nat_<'T> : Parser<'T, int64> = digit.SkipSome() :?> Parser<_, _> |> slice |>> int64
  let nat<'T> : Parser<'T, int64> = token (fun () -> nat_)
  let tailChar<'T> : Parser<'T, char> =
    satisfy (fun c -> Char.IsLetter(c) || Char.IsDigit(c) || c = '_' || c = '#' || c = '\'')
  let rawTailChar<'T> : Parser<'T, char> =
    rawSatisfy (fun c -> Char.IsLetter(c) || Char.IsDigit(c) || c = '_' || c = '#' || c = '\'')
  let identTail<'T> : Parser<'T, unit> = tailChar.SkipMany() :?> Parser<'T, _>
  let rawIdentTail<'T> : Parser<'T, unit> = rawTailChar.SkipMany() :?> Parser<'T, _>

  let nonopChars = "()[]{};,\"" |> Seq.sort |> Seq.toArray
  let opChars = ":!#$%&*+./<=>?@\\^|-~'`" |> Seq.sort |> Seq.toArray

  // TODO: existsIn

  // TODO: Op module
