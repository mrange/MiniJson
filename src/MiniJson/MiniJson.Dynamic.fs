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

/// MiniJson aims to be a minimal yet conforming JSON parser with reasonable performance and decent error reporting
///   JSON Specification: http://json.org/
///   JSON Lint         : http://jsonlint.com/
///
/// MiniJson.DynamicJsonModule contains functionality query a JSON document using
/// op_Dynamic ( ? ) in F#
///
///
/// Example:
/// --------
///   let root = json.Query
///
///   for i = 0 to root.Length - 1 do
///     let v     = root.[i]
///     let id    = v?id.AsString
///     let name  = v?name.AsString
///     let age   = v?age.AsFloat
///     printfn "Record - %d: id:%s, name:%s, age:%f" i id name age
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

/// Represents a valid JSON element query
type JsonQuery =
  /// (name)  - Represents a valid JSON element object property query
  | QueryProperty of string
  /// (index) - Represents a valid JSON element array indexing query
  | QueryIndexOf  of int

/// Represents an invalid JSON element query
type JsonQueryError =
  /// (name)  - Represents a invalid JSON element object property query as the referenced element wasn't an object
  | ErrorNotObject        of string
  /// (name)  - Represents a invalid JSON element array indexing query as the referenced element wasn't an array
  | ErrorNotIndexable     of int
  /// (name)  - Represents a invalid JSON element object property query as the property didn't exist
  | ErrorUnknownProperty  of string
  /// (name)  - Represents a invalid JSON element array indexing query as the index was out of bounds
  | ErrorIndexOutBounds   of int

/// (json, parents) - Represents a valid Path to a JSON element
type Path         = Json*(JsonQuery*Json) list

/// (errors, json, parents) - Represents an invalid Path to a JSON element
type InvalidPath  = JsonQueryError list*Json*(JsonQuery*Json) list

module internal Details =
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
/// Represents a JSON scalar (null, bool, number, string or error)
type JsonScalar =
  /// (json, parents)         - Represents a null scalar
  | ScalarNull        of Path
  /// (json, parents)         - Represents a bool scalar
  | ScalarBoolean     of Path*bool
  /// (json, parents)         - Represents a number (float) scalar
  | ScalarNumber      of Path*float
  /// (json, parents)         - Represents a string scalar
  | ScalarString      of Path*string
  /// (json, parents)         - An errors representing that the referenced element is not a scalar
  | ScalarNotScalar   of Path
  /// (json, parents)         - An errors representing that the referenced element doesn't exist
  | ScalarInvalidPath of InvalidPath

  /// Returns true if it's an invalid scalar element
  member x.IsError : bool =
    match x with
    | ScalarNull        _
    | ScalarBoolean     _
    | ScalarNumber      _
    | ScalarString      _ -> false
    | ScalarNotScalar   _
    | ScalarInvalidPath _ -> true

  /// Returns true if it's a valid scalar element,
  ///   if the scalar element couldn't be converted successfully returns false.
  member x.HasValue : bool =
    match x with
    | ScalarNull        _ -> false
    | ScalarBoolean     _
    | ScalarNumber      _
    | ScalarString      _ -> true
    | ScalarNotScalar   _
    | ScalarInvalidPath _ -> false

  /// Returns a boolean representation of the scalar element,
  ///   if the scalar element couldn't be converted successfully returns false.
  member x.AsBool : bool =
    match x with
    | ScalarNull        _     -> false
    | ScalarBoolean     (_,b) -> b
    | ScalarNumber      (_,n) -> n <> 0.0
    | ScalarString      (_,s) -> s.Length > 0
    | ScalarNotScalar   _
    | ScalarInvalidPath _     -> false

  /// Returns a float representation of the scalar element,
  ///   if the scalar element couldn't be converted successfully returns 0.0.
  member x.AsFloat : float =
    x.ConvertToFloat 0.

  /// Returns a string representation of the scalar element.
  ///   Null values and errors are represented as ""
  member x.AsString : string =
    x.AsStringImpl false

  /// Returns the expanded string representation of the scalar element.
  ///   Expanded means null values are represented as 'null' and full error error infos are generated for errors
  member x.AsExpandedString : string =
    x.AsStringImpl true

  /// Returns a float representation of the scalar element.
  ///   This allows the user to specify the value to return if the scalar element couldn't be converted successfully
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

  member internal x.AsStringImpl (expand : bool) : string =
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
      x.AsStringImpl true

/// Represents a path to a JSON element
type JsonPath =
  /// (json, parents)         - Holds the current json element and its parents
  | PathOk    of Path
  /// (errors, json, parents) - Holds the invalid path elements, the last valid json element and its parents
  | PathError of InvalidPath

  /// Evaluates the path producing the referenced scalar element (scalar meaning null, bool, number, string or error)
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

  /// Returns the Length of the referenced array element,
  ///   if it's not an array returns 0
  member x.Length : int =
    match x with
    | PathOk (json, _) ->
      match json with
      | JsonNull
      | JsonBoolean _
      | JsonNumber  _
      | JsonString  _   -> 0
      | JsonObject  ms  -> ms.Length
      | JsonArray   vs  -> vs.Length
    | PathError _       -> 0

  /// Returns a path to the element at the index of the referenced array element,
  ///   if it's not an array or out of bounds returns a PathError
  member x.Item (i : int) : JsonPath =
    match x with
    | PathOk (json, parents) ->
      match json with
      | JsonNull
      | JsonBoolean _
      | JsonNumber  _
      | JsonString  _   ->
        PathError ([ErrorNotIndexable i], json, parents)
      | JsonObject  ms  ->
        if i >= 0 && i < ms.Length then
          let _, v = ms.[i]
          PathOk (v, (QueryIndexOf i, json)::parents)
        else
          PathError ([ErrorIndexOutBounds i], json, parents)
      | JsonArray   vs  ->
        if i >= 0 && i < vs.Length then
          let v = vs.[i]
          PathOk (v, (QueryIndexOf i, json)::parents)
        else
          PathError ([ErrorIndexOutBounds i], json, parents)
    | PathError (errors, json, parents) ->
      PathError ((ErrorNotIndexable i)::errors, json, parents)


  /// Returns all property names in order (and with potential duplicates)
  ///   if it's not an object or named element doesn't exists returns an empty array
  member x.Names : string [] =
    match x with
    | PathOk (json, parents) ->
      match json with
      | JsonNull
      | JsonBoolean _
      | JsonNumber  _
      | JsonString  _
      | JsonArray   _   ->
        [||]
      | JsonObject  ms  ->
        ms |> Array.map (fun (k,_) -> k)
    | PathError (errors, json, parents) ->
      [||]

  /// Returns all children as an array of values in order (and with potential duplicates)
  ///   if it's not an object, array or named element doesn't exists returns an empty array
  member x.Children : JsonPath [] =
    match x with
    | PathOk (json, parents) ->
      match json with
      | JsonNull
      | JsonBoolean _
      | JsonNumber  _
      | JsonString  _   ->
        [||]
      | JsonArray   vs  ->
        vs |> Array.mapi (fun i v -> PathOk (v, (QueryIndexOf i, json)::parents))
      | JsonObject  ms  ->
        ms |> Array.mapi (fun i (_,v) -> PathOk (v, (QueryIndexOf i, json)::parents))
    | PathError (errors, json, parents) ->
      [||]


  /// Returns a path to the named element of the referenced object element,
  ///   if it's not an object or named element doesn't exists returns a PathError
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

  /// Returns true if the path elements references a valid scalar element,
  ///   if the referenced scalar element couldn't be converted successfully returns false.
  member x.HasValue : bool =
    x.Eval.HasValue

  /// Returns a boolean representation of the referenced scalar element,
  ///   if the referenced scalar element couldn't be converted successfully returns false.
  member x.AsBool : bool =
    x.Eval.AsBool

  /// Returns a float representation of the referenced scalar element,
  ///   if the referenced scalar element couldn't be converted successfully returns 0.0.
  member x.AsFloat : float =
    x.Eval.AsFloat

  /// Returns a string representation of the referenced scalar element,
  ///   null values and errors are represented as "".
  member x.AsString : string =
    x.Eval.AsString

  /// Returns the expanded string representation of the referenced scalar element.
  ///   Expanded means null values are represented as 'null' and full error error infos are generated for errors.
  member x.AsExpandedString : string =
    x.Eval.AsExpandedString

  /// Returns a float representation of the referenced scalar element.
  ///   This allows the user to specify the value to return if the referenced scalar element couldn't be converted successfully.
  ///   @defaultTo  - The float to default to if the referenced scalar element couldn't be converted successfully.
  member x.ConvertToFloat (defaultTo : float) : float =
    x.Eval.ConvertToFloat defaultTo

  override x.ToString () : string =
    match x with
    | PathOk (_, parents) ->
      let sb = StringBuilder ("PathOk: root")
      appendParents sb parents
      sb.ToString ()
    | PathError (errors, _, parents) ->
      let sb = StringBuilder ("PathError: root")
      appendParents sb parents
      appendErrors  sb errors
      sb.ToString ()

  /// Returns a path to the named element of the referenced object element,
  ///   if it's not an object or named element doesn't exists returns a PathError
  static member inline ( ? ) (path : JsonPath, name : string) : JsonPath =
    path.Get name

  /// Evaluates the path producing the referenced scalar element (scalar meaning null, bool, number, string or error)
  static member inline ( !! ) (path : JsonPath) : JsonScalar =
    path.Eval

/// Creates a JsonPath object from a JSON document
///   @json - A JSON document
let inline makePath (json : Json) = PathOk (json, [])

type Json with

  /// Creates a JsonPath object from a JSON document
  member x.Query : JsonPath = makePath x

module Infixes =

  // These operators are provided as a workaround for regression in F#4.1
  //  https://github.com/Microsoft/visualfsharp/issues/2416
  //  (Fixed as 2017-04-27)

  /// Returns a path to the named element of the referenced object element,
  ///   if it's not an object or named element doesn't exists returns a PathError
  let inline ( ? ) (path : JsonPath) (name : string) : JsonPath =
    path.Get name

  /// Evaluates the path producing the referenced scalar element (scalar meaning null, bool, number, string or error)
  let inline ( !! ) (path : JsonPath) : JsonScalar =
    path.Eval
