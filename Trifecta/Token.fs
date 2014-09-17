namespace Trifecta

type Token =
  | VSemi
  | VBrace
  | WhiteSpace
  | Other
  with
    override this.ToString() =
      match this with
      | VSemi -> "virtual semicolon"
      | VBrace -> "virtual right brace"
      | WhiteSpace -> "white space"
      | Other -> "other"
