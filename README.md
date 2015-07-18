# MiniJson

Minimal conforming JSON parser for F#

The best way of referencing MiniJson is to use paket http references

paket.dependencies
```
http https://raw.githubusercontent.com/mrange/MiniJson/master/src/MiniJson/MiniJson.fs
```

Using MiniJson is straight-forward

```fsharp
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
  let jsonText = """{"Hello":"There"}"""

  match parse true jsonText with  // true for full error-info
  | Success json        -> printfn "Success\n%s"    <| toString true json  // true to indent JSON
  | Failure (msg, pos)  -> printfn "Failure@%d\n%s" pos msg
  0
```

# TODO

1. Publish nuget package
1. Support F# dynamic operator
1. Improve README.md
