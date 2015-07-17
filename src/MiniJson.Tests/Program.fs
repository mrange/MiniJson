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
open System

open MiniJson.JsonModule

let parseAndPrint (input : string) : unit =
  let r = parse true input
  match r with
  | Success v ->
    printfn "-- \nInput : %s\nPretty: %s\nAs    : %A" input (toString false v) v
  | Failure (desc, pos) ->
    printfn "Failed to parse : %s\nPos: %d\n%s" input pos desc


let print doIndent json =
  printfn "-- JSON --\n%s" (toString doIndent json)

let testSerializer () =
  let testCases =
    let arr = JsonArray [JsonNull; JsonString "Hello"; JsonBoolean false]
    let obj = JsonObject  ["x",JsonNull; "Hello\n",JsonString "Hello"; "Tetris", arr]

    [|
      JsonNull
      JsonBoolean true
      JsonString  "Hello\r\nThere!"
      JsonNumber  10.34
      JsonArray   []
      JsonObject  []
      JsonArray   [JsonNull]
      arr
      JsonArray   [JsonNull; arr; JsonBoolean false]
      obj
      JsonObject  ["x",JsonNull; "Hello\n",JsonString "Hello"; "Tetris", arr; "Obj", obj]
    |]

  for testCase in testCases do
    print true testCase

let testUnserializer () =
  parseAndPrint """[0]"""
  parseAndPrint """[-0]"""
  parseAndPrint """[123]"""
  parseAndPrint """[-123]"""
  parseAndPrint """[1.23]"""
  parseAndPrint """[-12.3]"""
  parseAndPrint """[1.23E2]"""
  parseAndPrint """[-12.3E-2]"""
  parseAndPrint """[-12.3e+2]"""
  parseAndPrint """[null]"""
  parseAndPrint """[true]"""
  parseAndPrint """[false]"""
  parseAndPrint """["Hello\r\nThere"]"""
  parseAndPrint """["Hello\u004a"]"""
  parseAndPrint """[false,true,null]"""
  parseAndPrint """ [ false ,true, null ] """
  parseAndPrint """[[], null, [true]]"""

  parseAndPrint """[-01]"""
  parseAndPrint """[0123]"""
  parseAndPrint """[1.]"""
  parseAndPrint """[1E]"""
  parseAndPrint """["Hello]"""
  parseAndPrint """["Hello\xThere"]"""
  parseAndPrint """["Hello\u00"]"""

[<EntryPoint>]
let main argv =
  //testSerializer ()
  testUnserializer ()

  0
