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
module MiniJson.Tests.TestCases
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
open System
open System.IO

open MiniJson.Tests.Test
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
let positiveTestCases =
  [|
    """[0]"""
    """[-0]"""
    """[0.125]"""
    """[-0.125]"""
    """[0e2]"""
    """[-0E2]"""
    """[0.125E3]"""
    """[-0.125E2]"""
    """[125]"""
    """[-125]"""
    """[1.25]"""
    """[-12.5]"""
    """[1.25E2]"""
    """[-12.5E-2]"""
    """[-12.5e+2]"""
    """[null]"""
    """[true]"""
    """[false]"""
    """["Hello\r\nThere"]"""
    """["Hello\u004a"]"""
    """["\"\\\/\b\f\n\r\t\u2665"]"""
    """["\u0123\u4567\u89AB\uCDEF"]"""
    """["\u0123\u4567\u89ab\ucdef"]"""
    """[false,true,null]"""
    """[ false ,true, null ]"""
    """[[], null, [true]]"""
    """{"abc":123}"""
    """{"abc" :123}"""
    """{ "abc":123}"""
    """{ "abc" :123}"""
    "\t[\rfalse \r\n, true\n]\t"
  |] |> Array.map (fun v -> true, sprintf "Positive: %s" v, v)

let noIndentOracles =
  [|
    """[0]"""
    """[0]"""
    """[0.125]"""
    """[-0.125]"""
    """[0]"""
    """[0]"""
    """[125]"""
    """[-12.5]"""
    """[125]"""
    """[-125]"""
    """[1.25]"""
    """[-12.5]"""
    """[125]"""
    """[-0.125]"""
    """[-1250]"""
    """[null]"""
    """[true]"""
    """[false]"""
    """["Hello\r\nThere"]"""
    """["HelloJ"]"""
    """["\"\\\/\b\f\n\r\t♥"]"""
    "[\"\u0123\u4567\u89AB\uCDEF\"]"
    "[\"\u0123\u4567\u89ab\ucdef\"]"
    """[false,true,null]"""
    """[false,true,null]"""
    """[[],null,[true]]"""
    """{"abc":123}"""
    """{"abc":123}"""
    """{"abc":123}"""
    """{"abc":123}"""
    "[false,true]"
  |]

let withIndentOracles =
  [|
    """[
  0
]"""

    """[
  0
]"""

    """[
  0.125
]"""

    """[
  -0.125
]"""

    """[
  0
]"""

    """[
  0
]"""

    """[
  125
]"""

    """[
  -12.5
]"""

    """[
  125
]"""

    """[
  -125
]"""

    """[
  1.25
]"""

    """[
  -12.5
]"""

    """[
  125
]"""

    """[
  -0.125
]"""

    """[
  -1250
]"""

    """[
  null
]"""

    """[
  true
]"""

    """[
  false
]"""

    """[
  "Hello\r\nThere"
]"""

    """[
  "HelloJ"
]"""

    """[
  "\"\\\/\b\f\n\r\t♥"
]"""

    "[\r\n  \"\u0123\u4567\u89AB\uCDEF\"\r\n]"
    "[\r\n  \"\u0123\u4567\u89ab\ucdef\"\r\n]"

    """[
  false,
  true,
  null
]"""

    """[
  false,
  true,
  null
]"""

    """[
  [
  ],
  null,
  [
    true
  ]
]"""

    """{
  "abc":
    123
}"""

    """{
  "abc":
    123
}"""

    """{
  "abc":
    123
}"""

    """{
  "abc":
    123
}"""

    """[
  false,
  true
]"""

  |]

let negativeTestCases =
  [|
    """null"""
    """true"""
    """false"""
    """0"""
    """-0"""
    """125"""
    """-125"""
    "\"Hello\""
    """[01]"""
    """[-01]"""
    """[0125]"""
    """[+0]"""
    """[+125]"""
    """[.1]"""
    """[-.0]"""
    """[1.]"""
    """[1E]"""
    """[1E+]"""
    """[1E-]"""
    """["Hello]"""
    """["Hello\xThere"]"""
    """["Hello\u"]"""
    """["Hello\u00"]"""
    """["Hello\uPQ"]"""
    """{abc:3}"""
    """{"abc:3}"""
    """{"abc":}"""
    """["Hello this is a somewhat wide errornous string, to demonstrate \ERROR window"]"""
    """["Hello this is a wide errornous string, to demonstrate error window capabilities... The window is around 60 chars so now is the time for \ERROR"]"""
    """["Hello this is a wide errornous string, to demonstrate error window capabilities, here is the \ERROR... The window is around 60 chars"]"""
    """["Early \ERROR, hello this is a wide errornous string, to demonstrate error window capabilities. The window is around 60 chars"]"""
    """[fals"""
    """[t"""
  |] |> Array.map (fun v -> false, sprintf "Negative: %s" v, v)

let errorReportingOracles =
  [|
    """Failed to parse input as JSON
null
^ Pos: 0
Expected: '[' or '{'"""

    """Failed to parse input as JSON
true
^ Pos: 0
Expected: '[' or '{'"""

    """Failed to parse input as JSON
false
^ Pos: 0
Expected: '[' or '{'"""

    """Failed to parse input as JSON
0
^ Pos: 0
Expected: '[' or '{'"""

    """Failed to parse input as JSON
-0
^ Pos: 0
Expected: '[' or '{'"""

    """Failed to parse input as JSON
125
^ Pos: 0
Expected: '[' or '{'"""

    """Failed to parse input as JSON
-125
^ Pos: 0
Expected: '[' or '{'"""

    """Failed to parse input as JSON
"Hello"
^ Pos: 0
Expected: '[' or '{'"""

    """Failed to parse input as JSON
[01]
--^ Pos: 2
Expected: ',', '.', 'E' or 'e'"""

    """Failed to parse input as JSON
[-01]
---^ Pos: 3
Expected: ',', '.', 'E' or 'e'"""

    """Failed to parse input as JSON
[0125]
--^ Pos: 2
Expected: ',', '.', 'E' or 'e'"""

    """Failed to parse input as JSON
[+0]
-^ Pos: 1
Expected: '"', '-', '[', '{', digit, false, null or true"""

    """Failed to parse input as JSON
[+125]
-^ Pos: 1
Expected: '"', '-', '[', '{', digit, false, null or true"""

    """Failed to parse input as JSON
[.1]
-^ Pos: 1
Expected: '"', '-', '[', '{', digit, false, null or true"""

    """Failed to parse input as JSON
[-.0]
--^ Pos: 2
Expected: '0' or digit"""   // TODO: Improve this error message by removing '0'

    """Failed to parse input as JSON
[1.]
---^ Pos: 3
Expected: digit"""

    """Failed to parse input as JSON
[1E]
---^ Pos: 3
Expected: '+', '-' or digit"""

    """Failed to parse input as JSON
[1E+]
----^ Pos: 4
Expected: digit"""

    """Failed to parse input as JSON
[1E-]
----^ Pos: 4
Expected: digit"""

    """Failed to parse input as JSON
["Hello]
--------^ Pos: 8
Unexpected: EOS"""

    """Failed to parse input as JSON
["Hello\xThere"]
--------^ Pos: 8
Expected: '"', '/', '\', 'b', 'f', 'n', 'r', 't' or 'u'"""

    """Failed to parse input as JSON
["Hello\u"]
---------^ Pos: 9
Expected: hexdigit"""

    """Failed to parse input as JSON
["Hello\u00"]
-----------^ Pos: 11
Expected: hexdigit"""

    """Failed to parse input as JSON
["Hello\uPQ"]
---------^ Pos: 9
Expected: hexdigit"""

    """Failed to parse input as JSON
{abc:3}
-^ Pos: 1
Expected: '"'"""

    """Failed to parse input as JSON
{"abc:3}
--------^ Pos: 8
Unexpected: EOS"""

    """Failed to parse input as JSON
{"abc":}
-------^ Pos: 7
Expected: '"', '-', '[', '{', digit, false, null or true"""

    """Failed to parse input as JSON
rnous string, to demonstrate \ERROR window"]
------------------------------^ Pos: 66
Expected: '"', '/', '\', 'b', 'f', 'n', 'r', 't' or 'u'"""

    """Failed to parse input as JSON
chars so now is the time for \ERROR"]
------------------------------^ Pos: 139
Expected: '"', '/', '\', 'b', 'f', 'n', 'r', 't' or 'u'"""

    """Failed to parse input as JSON
ow capabilities, here is the \ERROR... The window is around 6
------------------------------^ Pos: 96
Expected: '"', '/', '\', 'b', 'f', 'n', 'r', 't' or 'u'"""

    """Failed to parse input as JSON
["Early \ERROR, hello this is a wide errornous string, to dem
---------^ Pos: 9
Expected: '"', '/', '\', 'b', 'f', 'n', 'r', 't' or 'u'"""
    """Failed to parse input as JSON
[fals
-^ Pos: 1
Expected: '"', '-', '[', '{', digit, false, null or true"""
    """Failed to parse input as JSON
[t
-^ Pos: 1
Expected: '"', '-', '[', '{', digit, false, null or true"""
  |]


let sampleTestCases =
  try
    let path  = Path.Combine (AppDomain.CurrentDomain.BaseDirectory, "TestCases")
    let jsons = Directory.GetFiles (path, "*.json")

    jsons
    |> Array.map (fun json -> true, sprintf "Sample: %s" <| Path.GetFileName json, File.ReadAllText json)
  with
  | ex -> errorf "EXCEPTION: %s" ex.Message; [||]

// ----------------------------------------------------------------------------------------------
