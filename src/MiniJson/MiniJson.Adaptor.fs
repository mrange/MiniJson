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
/// MiniJson.Adaptor provides adaptor functions to simplify MiniJson usage for non-F# libraries
///
namespace MiniJson.Adaptor

open System
open System.Collections.Generic
open System.Diagnostics
open System.Dynamic
open System.Linq
open System.Runtime.Serialization
open System.Security.Permissions

open MiniJson.JsonModule
open MiniJson.DynamicJsonModule

/// JsonParseException (msg, pos) is raised during parser errors
exception JsonParseException of string*int
  with
    /// Gets the error message, the message describes any potential parse failures
    member x.ErrorMessage   : string  = x.Data0

    /// Gets the position where parsing stopped. Note the position might point beyond end of string
    member x.ErrorPosition  : int     = x.Data1

    override x.ToString ()  : string  = x.Data0

/// JsonDynamicObject implements DynamicObject which allows C# (and VB) to
///   explore JSON values using dynamic lookup
[<NoEquality>]
[<NoComparison>]
type JsonDynamicObject(jsonPath : JsonPath) =
  inherit DynamicObject()

  override x.ToString ()              : string      = jsonPath.AsString

  override x.GetDynamicMemberNames () : seq<string> = upcast jsonPath.Names

  override x.TryGetIndex (binder : GetIndexBinder, indexes : obj [], result : obj byref) : bool =
    let index =
      if indexes.Length = 1 then
        match indexes.[0] with
        | :? int as i -> i
        | _ -> Int32.MinValue
      else
        Int32.MinValue
    if index > Int32.MinValue then
      let next = jsonPath.[index]
      result <- JsonDynamicObject next
      true
    else
      base.TryGetIndex (binder, indexes, &result)

  override x.TryGetMember (binder : GetMemberBinder, result : obj byref) : bool =
    let next = jsonPath.Get binder.Name
    result <- JsonDynamicObject next
    true

  override x.TryConvert (binder : ConvertBinder, result : obj byref) : bool =
    let rt = binder.ReturnType
    if rt = typeof<string> then
      result <- jsonPath.AsString
      true
    elif rt = typeof<float> then
      result <- jsonPath.AsFloat
      true
    elif rt = typeof<bool> then
      result <- jsonPath.AsBool
      true
    else
      base.TryConvert (binder, &result)

  // These are methods not properties in order to not collide with the dynamic lookup of JSON properties

  /// Gets the JSON Path object
  member x.GetJsonPath ()       : JsonPath  = jsonPath

  /// Gets the JSON Path object as string
  member x.GetJsonPathString () : string    = jsonPath.ToString ()

  /// Gets the referenced JSON object
  member x.GetJson ()           : Json      =
    match jsonPath with
    | PathOk    (json, _) -> json
    | PathError _         -> JsonNull

  /// Gets the referenced JSON object as string
  member x.GetJsonString ()     : string    = (x.GetJson ()).ToString ()

  /// Gets the number of child items the referenced JSON object has
  member x.GetLength ()         : int       = jsonPath.Length

  /// Returns true if the referenced JSON object exists
  member x.HasValue             : bool      = jsonPath.HasValue

  /// Converts the referenced JSON object to float (double),
  ///   if conversion fails returns 'f'
  member x.ConvertToFloat f     : float     = jsonPath.ConvertToFloat f

  /// Returns children of referenced JSON object
  member x.GetChildren ()       : JsonDynamicObject [] =
    jsonPath.Children |> Array.map JsonDynamicObject


[<NoEquality>]
[<NoComparison>]
type JsonParser(input : string, extendedErrorInfo : bool) =
  let result = parse extendedErrorInfo input

  override x.ToString ()  : string  = sprintf "%A" result

  /// Returns true when parse succeeded
  member x.Success        : bool    = match result with Success _ -> true | _ -> false

  /// Returns true when parse failed
  member x.Failure        : bool    = not x.Success

  /// Gets the parsed JSON object, throws JsonParseException on parse failure
  member x.Result         : Json    =
    match result with
    | Success json        -> json
    | Failure (msg, pos)  -> raise (JsonParseException (msg, pos))

  /// Gets the parse message, the message describes any potential parse failures
  member x.ParseMessage   : string  =
    match result with
    | Success _           -> "No errors detected during parse"
    | Failure (msg, _)    -> msg

  /// Gets the position where parsing stopped. Note the position might point beyond end of string
  member x.ParsePosition  : int     =
    match result with
    | Success _           -> input.Length
    | Failure (_, pos)    -> pos

  /// Gets the parsed JSON object as DynamicObject, throws JsonParseException on parse failure
  member x.DynamicResult  : JsonDynamicObject =
    let json      = x.Result
    let jsonPath  = makePath json
    JsonDynamicObject jsonPath
