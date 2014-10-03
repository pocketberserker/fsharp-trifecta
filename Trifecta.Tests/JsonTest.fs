namespace Trifecta.Tests

open Trifecta
open Parser

type Json =
  | JObject of (string * Json) list
  | JArray of Json list
  | JString of string
  | JNumber of Choice<int64, float>
  | JBool of bool
  | JNull

module JsonTest =

  let rec json<'T> : Lazy<Parser<'T, Json>> = lazy begin
    (JObject <!> brace (commaSep ((fun x y -> (x, y)) <!> stringLiteral <* pstring ":" <*> json<'T>.Value))) <|>
    (JArray <!> bracket (commaSep json<'T>.Value)) <|>
    (JString <!> stringLiteral) <|>
    (JNumber <!> ((nat |>> Choice1Of2) <|> (doubleLiteral |>> Choice2Of2))) <|>
    (JBool <!> ((pstring "true" |>> (fun _ -> true)) <|> (pstring "false" |>> (fun _ -> false)))) <|>
    (pstring "null" |>> (fun _ -> JNull))
  end

  //let parse s vs = run s vs (json.Value <* eof)
