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
module MiniJson.Tests.JsonNet
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
open System
open System.Collections.Generic
open System.Diagnostics
open System.IO

open Newtonsoft.Json

open MiniJson.JsonModule
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
module Details =
  [<NoEquality>]
  [<NoComparison>]
  type JsonBuilder =
    | BuilderRoot    of Json ref
    | BuilderObject  of string ref*ResizeArray<string*Json>
    | BuilderArray   of ResizeArray<Json>

open Details

let parse (input : string) : ParseResult =
  use ss  = new StringReader(input)
  use jtr = new JsonTextReader(ss)

  try

    let context       = Stack<JsonBuilder> ()
    context.Push <| (BuilderRoot <| ref JsonNull)

    let push v        =
      context.Push v
      true

    let pop ()        =
      match context.Pop () with
      | BuilderRoot   vr      -> !vr
      | BuilderObject (_,ms)  -> JsonObject (ms.ToArray ())
      | BuilderArray  vs      -> JsonArray (vs.ToArray ())

    let setKey k      =
      match context.Peek () with
      | BuilderRoot   vr      -> ()
      | BuilderObject (rk,_)  -> rk := k
      | BuilderArray  vs      -> ()
      true

    let add v         =
      match context.Peek () with
      | BuilderRoot   vr      -> vr := v
      | BuilderObject (rk,ms) -> ms.Add (!rk, v)
      | BuilderArray  vs      -> vs.Add v
      true

    let defaultSize   = 4

    let inline str ()        = Convert.ToString jtr.Value
    let inline number ()     = Convert.ToDouble jtr.Value
    let inline boolean ()    = Convert.ToBoolean jtr.Value

    let rec loop () =
      if jtr.Read () then
        let result =
          match jtr.TokenType with
          | JsonToken.Boolean           -> add    <| (JsonBoolean <| boolean ())
          | JsonToken.Bytes             -> false
          | JsonToken.Comment           -> false
          | JsonToken.Date              -> false
          | JsonToken.EndArray          -> add    <| pop ()
          | JsonToken.EndConstructor    -> false
          | JsonToken.EndObject         -> add    <| pop ()
          | JsonToken.Float             -> add    <| (JsonNumber <| number ())
          | JsonToken.Integer           -> add    <| (JsonNumber <| number ())
          | JsonToken.None              -> false
          | JsonToken.Null              -> add    <| JsonNull
          | JsonToken.PropertyName      -> setKey <| str ()
          | JsonToken.Raw               -> false
          | JsonToken.StartArray        -> push (BuilderArray <| ResizeArray<_>(defaultSize))
          | JsonToken.StartConstructor  -> false
          | JsonToken.StartObject       -> push (BuilderObject <| (ref "", ResizeArray<_>(defaultSize)))
          | JsonToken.String            -> add    <| (JsonString <| str ())
          | JsonToken.Undefined
          | _ -> false
        result && loop () // TODO: Check is tail-recursive
      else
        true

    match loop () with
    | true  ->
      Debug.Assert (context.Count = 1)
      let (BuilderRoot root) = context.Pop ()
      Success !root
    | false ->
      Failure ("Parse failure", jtr.LinePosition)
  with
  | ex -> Failure ("Parse failure: " + ex.Message, jtr.LinePosition + jtr.LineNumber*1000)
