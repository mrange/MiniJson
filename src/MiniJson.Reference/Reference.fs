// ----------------------------------------------------------------------------------------------
// Copyright 2015 Mårten Rånge
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
// ----------------------------------------------------------------------------------------------
module MiniJson.ReferenceJsonModule
// ----------------------------------------------------------------------------------------------
open System
open System.Globalization
open System.Text
open System.Threading
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
module Details =
  open FParsec.Primitives
  open FParsec.CharParsers
  open MiniJson.JsonModule

  type UserState  = unit
  type Parser<'t> = Parser<'t, UserState>

  let pjson =
    let puint64 = puint64 <?> "digit"

    let parray  , rparray   = createParserForwardedToRef<Json, unit> ()

    let pobject , rpobject  = createParserForwardedToRef<Json, unit> ()

    let pnull               = stringReturn "null" JsonNull

    let pboolean            = stringReturn "true" (JsonBoolean true) <|> stringReturn "false" (JsonBoolean false)

    let prawuintf =
      let p =
        let fromDigit(ch : char)= float ch - float '0'
        let fold f d  = 10.*f + fromDigit d
        Inline.Many (fromDigit , fold, id, digit)

      pipe3 getPosition p getPosition (fun p ui n -> ui,(n.Index - p.Index))

    let puintf = prawuintf |>> fun (f, _) -> f

    let pnumber =
      let inline pow i = pown 10.0 i
      let pminus : Parser<float->float>=
        charReturn '-' (fun d -> -d)
        <|>% id
      let psign : Parser<float->float>=
        charReturn '+' id <|> charReturn '-' (fun d -> -d)
        <|>% id
      let pfrac =
        pipe2 (skipChar '.') prawuintf (fun _ (uf,c) -> uf * (pow (int -c)))
        <|>% 0.0
      let pexp =
        pipe3 (anyOf "eE") psign puintf (fun _ sign e -> pow (int (sign e)))
        <|>% 1.0
      let pzero =
        charReturn '0' 0.0
      pipe4 pminus (pzero <|> puintf) pfrac pexp (fun s i f e -> JsonNumber (s (i + f)*e))

    let prawstring =
      let phex =
        hex
        |>> fun ch ->
          if Char.IsDigit ch then int ch - int '0'
          elif ch >= 'A' && ch <= 'F' then int ch - int 'A' + 10
          elif ch >= 'a' && ch <= 'f' then int ch - int 'a' + 10
          else 0
      let pstring_token = skipChar '"'
      let pesc_token    = skipChar '\\'
      let pch   = satisfyL (function '"' | '\\' | '\n'| '\r' -> false | _ -> true) "char"
      let pech  =
        let psimple =
          anyOf "\"\\/bfnrt"
          |>> function
            | 'b' -> '\b'
            | 'f' -> '\f'
            | 'n' -> '\n'
            | 'r' -> '\r'
            | 't' -> '\t'
            | c   -> c
        let punicode =
          pipe5 (skipChar 'u') phex phex phex phex (fun _ v0 v1 v2 v3 ->
            let ui = (v0 <<< 12) + (v1 <<< 8) + (v2 <<< 4) + v3
            char ui
            )
        pesc_token
        >>. (psimple <|> punicode)
      let pstr  = manyChars (choice [pch; pech])
      between pstring_token pstring_token pstr

    let pstring = prawstring |>> JsonString

    let pvalue  = choice [pnull; pboolean; pnumber; pstring; parray; pobject] .>> spaces

    let ptk ch = skipChar ch .>> spaces

    let parr =
      let pvalues = sepBy pvalue (ptk ',') |>> fun vs -> JsonArray (vs |> List.toArray)
      between (ptk '[') (ptk ']') pvalues

    let pobj =
      let pmember =
        pipe3 (prawstring .>> spaces) (ptk ':') pvalue (fun k _ v -> k,v)
      let pmembers = sepBy  pmember (ptk ',') |>> fun ms -> JsonObject (ms |> List.toArray)
      between (ptk '{') (ptk '}') pmembers

    rparray   := parr
    rpobject  := pobj

    spaces >>. (pobject <|> parray) .>> spaces .>> eof
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
open FParsec
open FParsec.CharParsers
open FParsec.Primitives
// ----------------------------------------------------------------------------------------------
let parse (input : string) : MiniJson.JsonModule.ParseResult =
  match run Details.pjson input with
  | Success (v, _, _)   -> MiniJson.JsonModule.Success v
  | Failure (msg,err,_) -> MiniJson.JsonModule.Failure (msg, int err.Position.Index)
// ----------------------------------------------------------------------------------------------
