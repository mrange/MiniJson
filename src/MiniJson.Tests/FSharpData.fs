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
module MiniJson.Tests.FSharpData
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
open FSharp.Data

open MiniJson.JsonModule
// ----------------------------------------------------------------------------------------------
let rawParse (input : string) : JsonValue = JsonValue.Parse (input)

let rec convert = function
  | JsonValue.String  s       -> JsonString s
  | JsonValue.Number  n       -> JsonNumber n
  | JsonValue.Float   f       -> JsonNumber (decimal f)
  | JsonValue.Record  members -> JsonObject (members  |> Array.map (fun (k,v) -> k, convert v))
  | JsonValue.Array   values  -> JsonArray  (values   |> Array.map convert)
  | JsonValue.Boolean b       -> JsonBoolean b
  | JsonValue.Null            -> JsonNull

let parse (input : string) : ParseResult =
  try
    Success <| convert (rawParse input)
  with
  | ex -> Failure (ex.Message, 0)

let dummyParse (input : string) : ParseResult =
    ignore <| rawParse input
    Success <| JsonNull
// ----------------------------------------------------------------------------------------------
