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

/// MiniJson aims to be a minimal yet compliant JSON parser with reasonable performance and decent error reporting
///   JSON Specification: http://json.org/
///   JSON Lint         : http://jsonlint.com/
#if PUBLIC_MINIJSON
module MiniJson.DynamicJsonModule
#else
// Due to what seems to be an issue with the F# compiler preventing
//  access to internal operator ? from within the same assembly
//  define INTERNAL_MINIJSON_WORKAROUND to suppress internalizing of
//  MiniJson.
#if INTERNAL_MINIJSON_WORKAROUND
module Internal.MiniJson.DynamicJsonModule
#else
module internal Internal.MiniJson.DynamicJsonModule
#endif
#endif
open System
open System.Globalization
open System.Text

open JsonModule

type JsonQuery =
  | QueryProperty of string
  | QueryIndexOf  of int

type JsonQueryError =
  | ErrorNotObject        of string
  | ErrorNotIndexable     of int
  | ErrorUnknownProperty  of string
  | ErrorIndexOutBounds   of int

type Path         = Json*(JsonQuery*Json) list
type InvalidPath  = JsonQueryError list*Json*(JsonQuery*Json) list

module Details =
    let inline ch   (sb : StringBuilder) (c : char)    : unit = ignore <| sb.Append c
    let inline str  (sb : StringBuilder) (s : string)  : unit = ignore <| sb.Append s
    let inline ii   (sb : StringBuilder) (i : int)     : unit = ignore <| sb.Append i

    let rec appendParents (sb : StringBuilder) = function
      | []    -> ()
      | p::ps ->
        // Tail-recursiveness not so important here as we expect only a few parents
        appendParents sb ps
        match p with
        | (QueryProperty name, _) -> ch sb '.'; str sb name
        | (QueryIndexOf i, _)     -> str sb ".["; ii sb i; ch sb ']'

    let rec appendErrors (sb : StringBuilder)  = function
      | []    -> ()
      | e::es ->
        // Tail-recursiveness not so important here as we expect only a few errors
        appendErrors sb es
        match e with
        | ErrorNotObject name
        | ErrorUnknownProperty name -> ch sb '!'; str sb name
        | ErrorNotIndexable i
        | ErrorIndexOutBounds i     -> str sb "!["; ii sb i; ch sb ']'

open Details

[<NoEquality>]
[<NoComparison>]
type JsonScalar =
  | ScalarNull        of Path
  | ScalarBoolean     of Path*bool
  | ScalarNumber      of Path*float
  | ScalarString      of Path*string
  | ScalarNotScalar   of Path
  | ScalarInvalidPath of InvalidPath

  member x.IsError : bool =
    match x with
    | ScalarNull        _
    | ScalarBoolean     _
    | ScalarNumber      _
    | ScalarString      _ -> false
    | ScalarNotScalar   _
    | ScalarInvalidPath _ -> true

  member x.HasValue : bool =
    match x with
    | ScalarNull        _ -> false
    | ScalarBoolean     _
    | ScalarNumber      _
    | ScalarString      _ -> true
    | ScalarNotScalar   _
    | ScalarInvalidPath _ -> false

  member x.AsBool : bool =
    match x with
    | ScalarNull        _     -> false
    | ScalarBoolean     (_,b) -> b
    | ScalarNumber      (_,n) -> n <> 0.0
    | ScalarString      (_,s) -> s.Length > 0
    | ScalarNotScalar   _
    | ScalarInvalidPath _     -> false

  member x.AsFloat : float =
    x.ConvertToFloat 0.

  member x.AsString : string =
    x.ToString false

  member x.AsExpandedString : string =
    x.ToString true

  member x.ConvertToFloat (defaultTo : float) : float =
    match x with
    | ScalarNull        _     -> 0.
    | ScalarBoolean     (_,b) -> if b then 1. else 0.
    | ScalarNumber      (_,n) -> n
    | ScalarString      (_,s) ->
      let b,f = Double.TryParse (s, NumberStyles.Float, CultureInfo.InvariantCulture)
      if b then f else defaultTo
    | ScalarNotScalar   _
    | ScalarInvalidPath _     -> defaultTo

  member x.ToString (expand : bool) : string =
    match x with
    | ScalarNull      _             -> if expand then "null" else ""
    | ScalarBoolean   (_,b)         -> if b then "true" else "false"
    | ScalarNumber    (_,n)         -> n.ToString CultureInfo.InvariantCulture
    | ScalarString    (_,s)         -> s
    | ScalarNotScalar path          ->
      if expand then
        let json, parents = path
        let sb = StringBuilder ("NotScalar: root")
        appendParents sb parents
        sb.ToString ()
      else
        ""
    | ScalarInvalidPath invalidPath ->
      if expand then
        let errors, json, parents = invalidPath
        let sb = StringBuilder ("InvalidPath: root")
        appendParents sb parents
        appendErrors  sb errors
        sb.ToString ()
      else
        ""

  override x.ToString () : string =
      x.ToString true

type JsonPath =
  | PathOk    of Path
  | PathError of InvalidPath

  member x.Eval : JsonScalar =
    match x with
    | PathOk path ->
      let json, _ = path
      match json with
      | JsonNull      -> ScalarNull       path
      | JsonBoolean b -> ScalarBoolean    (path, b)
      | JsonNumber  n -> ScalarNumber     (path, n)
      | JsonString  s -> ScalarString     (path, s)
      | _             -> ScalarNotScalar  path
    | PathError invalidPath -> ScalarInvalidPath invalidPath

  member x.Length : int =
    match x with
    | PathOk (json, _) ->
      match json with
      | JsonNull
      | JsonBoolean _
      | JsonNumber  _
      | JsonString  _
      | JsonObject  _   -> 0
      | JsonArray   vs  -> vs.Length
    | PathError _       -> 0

  member x.Item (i : int) : JsonPath =
    match x with
    | PathOk (json, parents) ->
      match json with
      | JsonNull
      | JsonBoolean _
      | JsonNumber  _
      | JsonString  _
      | JsonObject  _   ->
        PathError ([ErrorNotIndexable i], json, parents)
      | JsonArray   vs  ->
        if i >= 0 && i < vs.Length then
          let v = vs.[i]
          PathOk (v, (QueryIndexOf i, json)::parents)
        else
          PathError ([ErrorIndexOutBounds i], json, parents)
    | PathError (errors, json, parents) ->
      PathError ((ErrorNotIndexable i)::errors, json, parents)

  member x.Get (name : string) : JsonPath =
    match x with
    | PathOk (json, parents) ->
      match json with
      | JsonNull
      | JsonBoolean _
      | JsonNumber  _
      | JsonString  _
      | JsonArray   _   ->
        PathError ([ErrorNotObject name], json, parents)
      | JsonObject  ms  ->
        let rec find i =
          if i < ms.Length then
            let k,v = ms.[i]
            if k = name then
              PathOk (v, (QueryProperty name ,json)::parents)
            else
              find (i + 1)
          else
            PathError ([ErrorUnknownProperty name], json, parents)
        find 0
    | PathError (errors, json, parents) ->
      PathError ((ErrorNotObject name)::errors, json, parents)

  member x.HasValue : bool =
    x.Eval.HasValue

  member x.AsBool : bool =
    x.Eval.AsBool

  member x.AsFloat : float =
    x.Eval.AsFloat

  member x.AsString : string =
    x.Eval.AsString

  member x.AsExpandedString : string =
    x.Eval.AsExpandedString

  member x.ConvertToFloat (defaultTo : float) : float =
    x.Eval.ConvertToFloat defaultTo

  override x.ToString () : string =
    match x with
    | PathOk (_, parents) ->
      let sb = StringBuilder ("Path: root")
      appendParents sb parents
      sb.ToString ()
    | PathError (errors, _, parents) ->
      let sb = StringBuilder ("Path: root")
      appendParents sb parents
      appendErrors  sb errors
      sb.ToString ()

  static member ( ? ) (path : JsonPath, name : string) : JsonPath =
    path.Get name

  static member ( !! ) (path : JsonPath) : JsonScalar =
    path.Eval

let inline makePath (json : Json) = PathOk (json, [])

type Json with
  member x.Query : JsonPath = makePath x