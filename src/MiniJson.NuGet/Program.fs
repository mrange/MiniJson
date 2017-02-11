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
open MiniJson.JsonModule
open MiniJson.DynamicJsonModule

[<EntryPoint>]
let main argv =
  let jsonText = """[{"id":"123", "name":"Mr. Big", "age":30}, {"id":"123", "name":"Mr. X"}]"""

  match parse true jsonText with  // true for full error-info
  | Failure (msg, pos)  -> printfn "Failure@%d\n%s" pos msg
  | Success json        ->
    printfn "Success\n%s" <| toString true json  // true to indent JSON

    let root = json.Query

    // TODO: F#4.1 workaround
    let inline ( ? ) (x : JsonPath) name = x.Get name

    for i = 0 to root.Length - 1 do
      let v     = root.[i]
      let id    = v?id.AsString
      let name  = v?name.AsString
      let age   = v?age.AsFloat
      printfn "Record - %d: id:%s, name:%s, age:%f" i id name age
  0
