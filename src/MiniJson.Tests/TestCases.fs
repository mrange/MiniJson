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
    // Positive testcases
    true,   """[0]"""
    true,   """[-0]"""
    true,   """[0.125]"""
    true,   """[-0.125]"""
    true,   """[0e2]"""
    true,   """[-0E2]"""
    true,   """[0.125E3]"""
    true,   """[-0.125E2]"""
    true,   """[125]"""
    true,   """[-125]"""
    true,   """[1.25]"""
    true,   """[-12.5]"""
    true,   """[1.25E2]"""
    true,   """[-12.5E-2]"""
    true,   """[-12.5e+2]"""
    true,   """[null]"""
    true,   """[true]"""
    true,   """[false]"""
    true,   """["Hello\r\nThere"]"""
    true,   """["Hello\u004a"]"""
    true,   """["\"\\\/\b\f\n\r\t\u2665"]"""
    true,   """["\u0123\u4567\u89AB\uCDEF"]"""
    true,   """["\u0123\u4567\u89ab\ucdef"]"""
    true,   """[false,true,null]"""
    true,   """ [ false ,true, null ] """
    true,   """[[], null, [true]]"""
  |]

let negativeTestCases =
  [|
    // Negative testcases
    false,  """null"""
    false,  """true"""
    false,  """false"""
    false,  """0"""
    false,  """-0"""
    false,  """125"""
    false,  """-125"""
    false,  """ "Hello" """
    false,  """[01]"""
    false,  """[-01]"""
    false,  """[0125]"""
    false,  """[+0]"""
    false,  """[+125]"""
    false,  """[1.]"""
    false,  """[1E]"""
    false,  """[1E+]"""
    false,  """[1E-]"""
    false,  """["Hello]"""
    false,  """["Hello\xThere"]"""
    false,  """["Hello\u"]"""
    false,  """["Hello\u00"]"""
    false,  """["Hello\uPQ"]"""
  |]

let explicitTestCases =
  try
    let path  = Path.Combine (AppDomain.CurrentDomain.BaseDirectory, "TestCases")
    let jsons = Directory.GetFiles (path, "*.json")

    jsons
    |> Array.map (fun p -> true, File.ReadAllText p)
  with
  | ex -> errorf "EXCEPTION: %s" ex.Message; [||]

// ----------------------------------------------------------------------------------------------
