namespace Trifecta

open System
open FSharp.Karma

[<AbstractClass>]
type Located() =
  abstract member Loc: Loc
  abstract member Report: Document * Document [] -> Document
  default this.Report(msg: Document, [<ParamArray>] rest: Document []): Document =
    this.Loc.Report(msg, rest)
  member this.Die(msg: Document, [<ParamArray>] rest: Document []) =
    raise <| Death(this.Loc.Report(msg, rest))
  member this.Error(msg: Document, [<ParamArray>] rest: Document []) =
    failwith <| this.Loc.Report(msg, rest).ToString()
  abstract member Message: string -> string
  default this.Message(msg) =
    this.Loc.Message(msg)

and [<AbstractClass>] Loc() =
  inherit Located()
  override this.Loc = this
  abstract member OrElse: Loc -> Loc
  member this.UnifiedWith(l: Loc) = this.OrElse(l)
  abstract member Inferred: Loc
  default this.Inferred = this
  member this.InstantiatedBy(l: Loc) = this.OrElse(l)
  member this.Checked = this
  member this.Generalized = this

type Relocatable<'T when 'T :> Located> =
  abstract member SetLoc: 'T * Loc -> 'T

type Pos(fileName: string, current: string, line: int, column: int, ending: bool) =
  inherit Loc()
  member this.FileName = fileName
  member this.Current = current
  member this.Line = line
  member this.Column = column
  member this.Ending = ending
  member this.Bump(c: char, i: string, o: int) =
    if c = '\n' then
      let ix = i.IndexOf('\n', o)
      if ix = -1 then Pos(fileName, i.Substring(o), line + 1, 1, true)
      else Pos(fileName, i.Substring(o, ix), line + 1, 1, false)
    elif c = '\t' then Pos(fileName, current, line, column + (8 - column % 8), ending)
    else Pos(fileName, current, line, column + 1, ending)
  override this.Report(msg, [<ParamArray>] rest: Document []) =
    (((((DocText fileName @@ ":") @@ string line) @@ ":") @@ string column) @@ ":" <+> msg) ::
    (DocText current @@ (if ending then "<EOF>" else "")) ::
    (DocText (String.replicate (column - 1) " ") @@ "^") ::
    (List.ofArray rest)
    |> List.ofStdList
    |> Document.vsep List.traverse
  override this.Inferred = Inferred(this) :> Loc
  override this.ToString() = fileName + "(" + string line + ":" + string column + ")"
  override this.GetHashCode() = hash (fileName, line, column)
  override this.Equals(a) =
    match a with
    | :? Pos as p when fileName = p.FileName && line = p.Line && column = p.Column && ending = p.Ending -> true
    | _ -> false
  override this.Message(s) = s + " at " + fileName + ":" + string line + ":" + string column
  override this.OrElse(_) = this :> Loc

and Inferred(p: Pos) =
  inherit Loc()
  override this.Report(msg, [<ParamArray>] rest: Document []) =
    p.Report(msg <+> "inferred from", rest)
  override this.ToString() = "inferred from " + p.ToString()
  override this.OrElse(l) =
    match l with
    | :? Pos -> l
    | _ -> this :> Loc

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Loc =

  let relocatable = { new Relocatable<Loc> with
    member this.SetLoc(old, loc) = loc }

  let builtin = { new Loc() with
    override this.Report(msg, [<ParamArray>] rest: Document []) =
      Document.vsep List.traverse (msg :: List.ofArray rest |> List.ofStdList)
    member this.OrElse(l) = l
    override this.ToString() = "-"
    override this.Message(s) = s + "(builtin)" }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Pos =

  let start fileName contents =
    let current = System.String(contents |> Seq.takeWhile ((<>) '\n') |> Seq.toArray)
    Pos(fileName, current, 1, 1, String.length current = String.length contents)

  let order = { new Order<Pos>() with
    member this.Order(p, q) =
      let a = compare p.FileName q.FileName |> Ordering.fromInt
      let b = compare p.Line q.Line |> Ordering.fromInt
      let c = compare p.Column q.Column |> Ordering.fromInt
      Ordering.monoid.Append(Ordering.monoid.Append(a, b), c) }
