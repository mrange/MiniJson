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
module MiniJson.Tests.Test
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
open System
open Microsoft.FSharp.Core.Printf
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
let mutable errors = 0

let print (cc : ConsoleColor) (prelude : string) (msg : string) : unit =
  let p = Console.ForegroundColor
  try
    Console.ForegroundColor <- cc
    Console.Write prelude
    Console.WriteLine msg
  finally
    Console.ForegroundColor <- p

let error     msg     =
  errors <- errors + 1
  print ConsoleColor.Red    "ERROR  : " msg

let warning   msg     = print ConsoleColor.Yellow "WARNING: " msg
let info      msg     = print ConsoleColor.Gray   "INFO   : " msg
let success   msg     = print ConsoleColor.Green  "SUCCESS: " msg
let highlight msg     = print ConsoleColor.White  "HILIGHT: " msg

let errorf      f     = kprintf error     f
let warningf    f     = kprintf warning   f
let infof       f     = kprintf info      f
let successf    f     = kprintf success   f
let highlightf  f     = kprintf highlight f

let test_failure msg  = errorf "TEST: %s" msg
let test_failuref f   = kprintf test_failure f

let test_eq e a nm    =
  if e = a then true
  else
    errorf "TEST_EQ: %A = %A (%s)" e a nm
    false

let test_gt e a nm    =
  if e > a then true
  else
    errorf "TEST_GT: %A > %A (%s)" e a nm
    false

let test_lt e a nm    =
  if e < a then true
  else
    errorf "TEST_LT: %A < %A (%s)" e a nm
    false

let check_eq e a nm   = ignore <| test_eq e a nm
let check_gt e a nm   = ignore <| test_gt e a nm
let check_lt e a nm   = ignore <| test_lt e a nm


// ----------------------------------------------------------------------------------------------
