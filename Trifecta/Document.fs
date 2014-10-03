namespace Trifecta

open System.IO
open FSharp.Karma

type Document =
  | DocNil
  | DocNewline
  | DocBreak of hard: bool
  | DocText of txt: string
  | DocGroup of doc: Document
  | DocNest of indent: int * doc: Document
  | DocCons of hd: Document * tl: Document
  | DocColumn of (int -> Document)
  | DocNesting of (int -> Document)
  with
    // TODO: rename
    static member (@@)(tl: Document, hd: Document) = DocCons(hd, tl)
    static member (@@)(tl: Document, hd: string) = DocCons(DocText hd, tl)
    static member (@/@)(tl: Document, hd: Document) = hd @@ DocBreak true @@ tl
    static member (@/@)(tl: Document, hd: string) = DocText hd @@ DocBreak true @@ tl
    static member (</>)(tl: Document, hd: Document) = hd @@ DocGroup(DocBreak true) @@ tl
    static member (</>)(tl: Document, hd: string) = DocText hd @@ DocGroup(DocBreak true) @@ tl
    static member (@//@)(tl: Document, hd: Document) = hd @@ DocBreak false @@ tl
    static member (@//@)(tl: Document, hd: string) = DocText hd @@ DocBreak false @@ tl
    static member (<+>)(tl: Document, hd: Document) = hd @@ DocText " " @@ tl
    static member (<+>)(tl: Document, hd: string) = DocText hd @@ DocText " " @@ tl

type FmtState = int * bool * Document

type Death(error: Document, base_: exn) =
  inherit exn(error.ToString(), base_)
  new (error: Document) = Death(error, null)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Document =

  let empty = DocNil
  let space = DocText " "
  let hardline = DocNewline

  let dbreak = DocBreak true
  let line = DocBreak true

  let column f = DocColumn f
  let nesting f = DocNesting f

  let group doc = DocGroup doc
  
  let nest i d = DocNest(i, d)

  let above (xs: Document) (doc: Document) = doc @@ DocNewline @@ xs
  let align doc = column (fun k -> nesting(fun i -> nest (k - i) doc))

  let lowerOrdinal n (singular: Document) (plural: Document) =
    match n with
    | 0 -> DocText "no" <+> plural
    | 1 -> DocText "one" <+> singular
    | 2 -> DocText "two" <+> plural
    | 3 -> DocText "three" <+> plural
    | 4 -> DocText "four" <+> plural
    | 5 -> DocText "five" <+> plural
    | 6 -> DocText "six" <+> plural
    | _ -> DocText (string n) <+> plural

  let upperOrdinal n (singular: Document) (plural: Document) =
    match n with
    | 0 -> DocText "No" <+> plural
    | 1 -> DocText "One" <+> singular
    | 2 -> DocText "Two" <+> plural
    | 3 -> DocText "Three" <+> plural
    | 4 -> DocText "Four" <+> plural
    | 5 -> DocText "Five" <+> plural
    | 6 -> DocText "Six" <+> plural
    | _ -> DocText (string n) <+> plural

  let fold F f z =
    let m = Option.monoid (Semigroup.instance f)
    Foldable.foldMap F m Some z
    |> Option.getOrElse empty

  // TODO: use Dual
  let foldl F f z =
    z
    |> Foldable.foldl F (fun od d -> od |> Option.map (flip f d) |> Option.orElse (fun () -> Some d)) None
    |> Option.getOrElse empty

  let fillSep F z = fold F (</>) z
  let fillSepl F z = foldl F (fun a b -> b @@ group line @@ a) z
  let hsep F z = fold F (<+>) z
  let vsep F z = fold F above z
  let vcat F z = fold F (@/@) z
  let cat (xs: #_1<List, Document>) = group (vcat List.traverse xs)

  let punctuate (sep: Document) xs =
    let rec inner acc sep = function
      | [] -> acc
      | [d] -> [d]
      | d::ds -> inner ((d @@ sep) :: acc) sep ds
    inner [] sep xs

  let oxford (andOr: Document) = function
    | [] -> empty
    | [a] -> a
    | [a; b] -> a <+> andOr <+> b
    | xs ->
      fillSep List.traverse (punctuate (DocText ",") (Seq.take (List.length xs - 1) xs |> Seq.toList) |> List.ofStdList) @@ "," <+> andOr <+> Seq.last xs

  let spaces (writer: TextWriter) n =
    let mutable rem = n
    while rem >= 16 do
      writer.Write("                ")
      rem <- rem - 16
    if rem >= 8 then
      writer.Write("        ")
      rem <- rem - 8
    if rem >= 4 then
      writer.Write("    ")
      rem <- rem - 4
    if rem >= 2 then
      writer.Write("  ")
      rem <- rem - 2
    if rem = 1 then
      writer.Write(" ")

  let format width (writer: TextWriter) doc =
    let rec fits w (state: FmtState list) =
      match state with
      | _ when w < 0 -> false
      | [] -> true
      | (_, _, DocNil)::z -> fits w z
      | (i, b, DocCons(h, t))::z -> fits w ((i, b, h) :: (i, b, t) :: z)
      | (_, _, DocText t)::z -> fits (w - String.length t) z
      | (i, b, DocNest(ii, d))::z -> fits w ((i + ii, b, d) :: z)
      | (_, false, DocBreak true)::z -> fits (w - 1) z
      | (_, false, DocBreak false)::z -> fits w z
      | (_, true, DocBreak _)::_ -> true
      | (_, _, DocNewline)::_ -> true
      | (i, true, DocGroup d)::z -> fits w ((i, false, d) :: z) || fits w ((i, true, d) :: z)
      | (i, false, DocGroup d)::z -> fits w ((i, false, d) :: z)
      | (i, b, DocColumn f)::z -> fits w ((i, b, f (width - w)) :: z)
      | (i, b, DocNesting f)::z -> fits w ((i, b, f i) :: z)
    let rec fmt k (state: FmtState list) =
      match state with
      | [] -> ()
      | (_, _, DocNil)::z -> fmt k z
      | (i, b, DocCons(h, t))::z ->
        fmt k ((i, b, h) :: (i, b, t) :: z)
      | (i, _, DocText(t)) :: z ->
        writer.Write(t)
        fmt (k + String.length t) z
      | (i, b, DocNest(ii, d))::z ->
        fmt k ((i + ii, b, d) :: z)
      | (i, true, DocBreak _)::z ->
        writer.Write("\n")
        spaces writer i
        fmt i z
      | (i, false, DocBreak(true))::z ->
        writer.Write(" ")
        fmt (k + 1) z
      | (i, false, DocBreak false)::z -> fmt k z
      | (i, _, DocNewline)::z ->
        writer.Write("\n")
        spaces writer i
        fmt i z
      | (i, b, DocGroup d)::z ->
        let fitsFlat = fits (width - k) ((i, false, d) :: z)
        fmt k ((i, not fitsFlat, d) :: z)
      | (i, b, DocColumn f)::z -> fmt k ((i, b, f(k)) :: z)
      | (i, b, DocNesting f)::z -> fmt k ((i, b, f(i)) :: z)
    fmt 0 [(0, false, DocGroup doc)]

  let toString doc =
    use w = new StringWriter()
    format 80 w doc
    w.ToString()

[<AutoOpen>]
module DocumentOps =

  type Document with
    member this.ToString() = Document.toString this
