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
open Internal.MiniJson.JsonModule

[<EntryPoint>]
let main argv =
  match parse true """{"Hello":"There"}""" with
  | Success json        -> printfn "Success: %s"    <| toString false json
  | Failure (msg, pos)  -> printfn "Failure@%d\n%s" pos msg
  0
