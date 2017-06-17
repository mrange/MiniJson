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

// ----------------------------------------------------------------------------------------------
open System
open System.Diagnostics
open System.Globalization
open System.IO
open System.Runtime.InteropServices
open System.Text

open MiniJson
open MiniJson.JsonModule
open MiniJson.DynamicJsonModule
// These operators are provided as a workaround for the possible regression in F#4.1
open MiniJson.DynamicJsonModule.Infixes
open MiniJson.Adaptor
open MiniJson.Tests.Test
open MiniJson.Tests.TestCases
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
let jsonAsString (random : Random) (json : Json) : string =
  let sb = StringBuilder ()

  let inline str (s : string)     = ignore <| sb.Append s
  let inline ch  (c : char)       = ignore <| sb.Append c
  let inline num (f : float)      =
    if Double.IsNaN f then
      ignore <| sb.AppendFormat "\"NaN\""   // JSON numbers doesn't support NaN
    elif Double.IsPositiveInfinity f then
      ignore <| sb.AppendFormat "\"+Inf\""  // JSON numbers doesn't support +Inf
    elif Double.IsNegativeInfinity f then
      ignore <| sb.AppendFormat "\"-Inf\""  // JSON numbers doesn't support -Inf
    else
      ignore <| sb.AppendFormat (CultureInfo.InvariantCulture, "{0:G}", f)
  let ws ()                       =
    let e = random.Next(-1,3)
    for i = 0 to e do
      match random.Next(0,6) with
      | 0 -> ch '\n'
      | 1 -> ch '\r'
      | 2 -> ch '\t'
      | _ -> ch ' '

  let nonPrintableChars =
    [|
      for i in 0..31 ->
        match char i with
        | '\b'            -> @"\b"
        | '\f'            -> @"\f"
        | '\n'            -> @"\n"
        | '\r'            -> @"\r"
        | '\t'            -> @"\t"
        | ch              -> sprintf "\u%04X" i
    |]

  let estr (s : string) =
    ch '"'
    let e = s.Length - 1
    for i = 0 to e do
      match s.[i] with
      | '\"'            -> str @"\"""
      | '\\'            -> str @"\\"
      | '/'             -> str @"\/"
      | c when c < ' '  -> str nonPrintableChars.[int c]
      | c               -> ch c
    ch '"'

  let values b e (vs : 'T array) (a : 'T -> unit) =
    ch b
    ws ()
    let ee = vs.Length - 1
    for i = 0 to ee do
      let v = vs.[i]
      a v
      if i < ee then
        ch ','
        ws ()
    ch e
    ws ()

  let rec impl j =
    match j with
    | JsonNull          -> str "null"               ; ws ()
    | JsonBoolean true  -> str "true"               ; ws ()
    | JsonBoolean false -> str "false"              ; ws ()
    | JsonNumber n      -> num n                    ; ws ()
    | JsonString s      -> estr s                   ; ws ()
    | JsonArray vs      -> values '[' ']' vs impl   ; ws ()
    | JsonObject ms     -> values '{' '}' ms implkv ; ws ()
  and implkv (k,v) =
    estr k
    ws ()
    ch ':'
    ws ()
    impl v

  ws ()
  impl json

  sb.ToString ()
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
let randomizeJson (n : int) (random : Random) : Json =
  let randomizeRawString  (n : int) : string =
    String (Array.init (random.Next (3, 10)) (fun _ -> char (random.Next(65,65+25))))

  let randomizeNull       (n : int) : Json = JsonNull
  let randomizeBool       (n : int) : Json = JsonBoolean (random.Next (0,2) = 1)
  let randomizeNumber     (n : int) : Json = JsonNumber (random.NextDouble () * 100000.)
  let randomizeNumber     (n : int) : Json = JsonNumber 1.
  let randomizeString     (n : int) : Json = JsonString (randomizeRawString (n - 1))
  let rec randomizeArray  (n : int) : Json =
    let vs = Array.init (random.Next (0, 5)) (fun _ -> randomize (n - 1))
    JsonArray vs
  and randomizeObject     (n : int) : Json =
    let ms = Array.init (random.Next (0, 5)) (fun _ -> randomizeRawString (n - 1), randomize (n - 1))
    JsonObject ms
  and randomize           (n : int) : Json =
    if n = 0 then randomizeNumber n
    else
      match random.Next(0,12) with
      | 0 | 1     -> randomizeNull n
      | 2 | 3     -> randomizeBool n
      | 4 | 5 | 6 -> randomizeNumber n
      | 7 | 8 | 9 -> randomizeString n
      | 10        -> randomizeArray n
      | _         -> randomizeObject n

  match random.Next(0,2) with
  | 0         -> randomizeArray n
  | _         -> randomizeObject n
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
let compareParsers
  (referenceParser  : string -> ParseResult   )
  (positive         : bool                    )
  (name             : string                  )
  (testCase         : string                  )
  (postProcess      : Json -> Json -> unit    ) : unit =
  let expected  = referenceParser testCase
  let actual    = parse false testCase

  match expected, actual with
  | Success e     , Success a     ->
    check_eq true  positive  name
    if test_eq e a name then postProcess e a
  | Failure (_,e) , Failure (_,a) ->
    check_eq false positive  name
    check_eq e     a         name
  | _             , _             ->
    test_failuref "Parsing mismatch '%s', expected:%A, actual: %A" name expected actual
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
let generatedTestCases =
  let count       = 1000
  let repeat i v  = toString i <| JsonArray [| for i in 1..count -> v |]

  let s = @"https:\/\/coursera-university-assets.s3.amazonaws.com\/21\/9a0294e2bf773901afbfcb5ef47d97\/Stanford_Coursera-200x48_RedText_BG.png"

  [|
    true  , "Lots of nulls   (noindent)"  , repeat false  <| JsonNull
    true  , "Lots of false   (noindent)"  , repeat false  <| JsonBoolean false
    true  , "Lots of true    (noindent)"  , repeat false  <| JsonBoolean true
    true  , "Lots of numbers (noindent)"  , repeat false  <| JsonNumber  123.456
    true  , "Lots of strings (noindent)"  , repeat false  <| JsonString  s
    true  , "Lots of nulls   (indent)"    , repeat true   <| JsonNull
    true  , "Lots of false   (indent)"    , repeat true   <| JsonBoolean false
    true  , "Lots of true    (indent)"    , repeat true   <| JsonBoolean true
    true  , "Lots of numbers (indent)"    , repeat true   <| JsonNumber  123.456
    true  , "Lots of strings (indent)"    , repeat true   <| JsonString  s
  |]
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
let random = Random 19740531
let randomizedTestCases = Array.init 1000 <| fun i ->
  randomizeJson 10 random |> fun json -> true, sprintf "Generated: %d" i,toString false json
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
let runFunctionalTestCases
  (category       : string                                                      )
  (parserComparer : bool -> string -> string -> (Json -> Json ->  unit) -> unit )
  (testCases      : (bool*string*string) []                                     )
  (dumper         : string -> unit                                              ) : unit =
  let noAction _ _ = ()

  for positive, name, testCase in testCases do
    dumper <| sprintf "---==> %s <==---" category
    dumper name
    dumper (if positive then "positive" else "negative")
    dumper testCase

    parserComparer positive name testCase <| fun e a ->
      let unindented  = toString false e
      let indented    = toString true e
      let dumped      = jsonAsString random e

      dumper unindented
      dumper indented
      dumper dumped

      parserComparer positive name unindented noAction
      parserComparer positive name indented   noAction
      parserComparer positive name dumped     noAction
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
let filterForReference (_,_,_) =
  true
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
let functionalReferenceTestCases (dumper : string -> unit) =
  let testCases =
    Array.concat [|positiveTestCases; negativeTestCases; sampleTestCases; generatedTestCases; randomizedTestCases|]
    |> Array.filter filterForReference

  infof "Running %d functional testcases (REFERENCE)..." testCases.Length

  runFunctionalTestCases
    "FUNCTIONAL TEST (REFERENCE)"
    (compareParsers ReferenceJsonModule.parse)
    testCases
    dumper
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
let filterForJsonNet (_,name,_) =
  match name with
  | "Sample: Dates.json"                  -> false  // JSON.NET parses dates, MiniJson doesn't (as JSON has no concept of Dates)
  | "Sample: GitHub.json"                 -> false  // JSON.NET fails to parse GitHub.Json (valid according to http://jsonlint.com)
  | "Sample: optionals.json"              -> false  // JSON.NET fails to parse optionals.Json (valid according to http://jsonlint.com)
  | "Positive: [1E+309]"                  -> false  // JSON.NET fails to parse big floats
  | "Positive: [-1E309]"                  -> false  // JSON.NET fails to parse big floats
  | _ when name.StartsWith ("Negative: ") -> false  // JSON.NET is more relaxed when parsing therefore negative testcases can't be compared
  | _ -> true
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
let functionalJsonNetTestCases (dumper : string -> unit) =
  let testCases =
    Array.concat [|positiveTestCases; negativeTestCases; sampleTestCases; generatedTestCases; randomizedTestCases |]
    |> Array.filter filterForJsonNet

  infof "Running %d functional testcases (JSON.NET)..." testCases.Length

  runFunctionalTestCases
    "FUNCTIONAL TEST (JSON.NET)"
    (compareParsers MiniJson.Tests.JsonNet.parse)
    testCases
    dumper
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
let filterForFSharpData (_,name,_) =
  match name with
  | "Sample: optionals.json"              -> false  // FSharp.Data difference with MiniJson most likely due to Date/Float parsing, TODO: investigate
  | "Positive: [1E+309]"                  -> false  // FSharp.Data fails to parse big floats
  | "Positive: [-1E309]"                  -> false  // FSharp.Data fails to parse big floats
  | _ when name.StartsWith ("Negative: ") -> false  // FSharp.Data is more relaxed when parsing therefore negative testcases can't be compared
  | _ -> true
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
let functionalFSharpDataTestCases (dumper : string -> unit) =
  let testCases =
    Array.concat [|positiveTestCases; negativeTestCases; sampleTestCases; generatedTestCases; randomizedTestCases |]
    |> Array.filter filterForFSharpData

  infof "Running %d functional testcases (FSHARP.DATA)..." testCases.Length

  runFunctionalTestCases
    "FUNCTIONAL TEST (FSHARP.DATA)"
    (compareParsers MiniJson.Tests.FSharpData.parse)
    testCases
    dumper
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
let filterForJil (_,name : string,_) =
  true
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
let functionalJilTestCases (dumper : string -> unit) =
  let testCases =
    Array.concat [|positiveTestCases; negativeTestCases; sampleTestCases; generatedTestCases; randomizedTestCases |]
    |> Array.filter filterForJil

  infof "Running %d functional testcases (JIL)..." testCases.Length

  runFunctionalTestCases
    "FUNCTIONAL TEST (JIL)"
    (compareParsers MiniJson.Tests.Jil.parse)
    testCases
    dumper
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
let filterForPerformance (_,name : string ,tc : string) =
  match name with
  | _ when name.StartsWith ("Negative: ") -> false  // Negative test cases aren't tested for performance
  | "Sample: contacts.json"               -> false  // contacts.json doesn't have enough complexity
  | _ -> tc.Length > 500                            // Allow all other JSON documents bigger than 500 char
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
type PerformanceData = string*int*int*int*int*int64*uint64

module InteropWithNative =
  [<DllImport("kernel32.dll")>]
  extern IntPtr GetCurrentThread ();

  [<DllImport("kernel32.dll")>]
  extern bool QueryThreadCycleTime (IntPtr ThreadHandle, uint64 * CycleTime);

let collectPerformanceData
  (category       : string                  )
  (parser         : string -> ParseResult   )
  (iterations     : int                     )
  (testCases      : (bool*string*string) [] )
  (dumper         : string -> unit          ) : PerformanceData [] =

  infof "Collecting performance data for %d testcases (%s)..." testCases.Length category

  let sw = Stopwatch ()

  let timeIt n (a : unit -> 'T) : int64*uint64*int*int*int*'T =
    sw.Reset ()

    let result = a ()

    GC.Collect (2, GCCollectionMode.Forced, true)

    let ct  = InteropWithNative.GetCurrentThread ()

    let b0  = GC.CollectionCount 0
    let b1  = GC.CollectionCount 1
    let b2  = GC.CollectionCount 2

    sw.Start ()

    let mutable bct = 0UL
    let mutable ect = 0UL
    ignore <| InteropWithNative.QueryThreadCycleTime (ct, &&bct)

    for i = 1 to n do
      ignore <| a ()

    ignore <| InteropWithNative.QueryThreadCycleTime (ct, &&ect)

    sw.Stop ()

    let e0  = GC.CollectionCount 0
    let e1  = GC.CollectionCount 1
    let e2  = GC.CollectionCount 2

    sw.ElapsedMilliseconds, ect - bct , e0 - b0, e1 - b1, e2 - b2, result

  [|
    for positive, name, testCase in testCases do
      dumper <| sprintf "---==> PERFORMANCE (%s) <==---" category
      dumper name
      dumper (if positive then "positive" else "negative")
      dumper testCase

      let time, ct, cc0, cc1, cc2 , _ = timeIt iterations (fun _ -> parser testCase)
      dumper <| sprintf "Iterations: %d, cc0: %d, cc1: %d, cc2: %d, time: %d ms" iterations cc0 cc1 cc2 time
      yield name, iterations, cc0, cc1, cc2, time, ct
  |]
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
let performanceTestCases (dumper : string -> unit) =

  use perfCsv = new StreamWriter("perf.csv", false, Encoding.UTF8)

  let allTtestCases =
    Array.concat [|positiveTestCases; negativeTestCases; sampleTestCases; (*generatedTestCases;*) randomizedTestCases |]

  let miniJsonData =
    let testCases =
      allTtestCases
      |> Array.filter filterForPerformance

    collectPerformanceData
      "MINIJSON"
      (parse false)
      1000
      testCases
      dumper

  let writeField (f : string) =
    perfCsv.Write (f.Replace(',', ' '))
  let writeDelimiter () =
    perfCsv.Write ","
  let writeNewLine () =
    perfCsv.WriteLine ()

  writeField "parser"
  for testCase, _, _, _, _, _, _ in miniJsonData do
    writeDelimiter ()
    writeField testCase
  writeNewLine ()

  let expectedRatio v = max 1.0 v

  let writeResults
    (name             : string            )
    (data             : PerformanceData[] ) =
    infof "Writing performance data for %s" name

    writeField name

    for testCase0, iterations0, cc00, cc10, cc20, time0, ct0 in miniJsonData do
      writeDelimiter ()
      match data |> Array.tryFind (fun (testCase1, _, _, _, _, _, _) -> testCase0 = testCase1) with
      | None ->
        writeField "-"
      | Some (_, iterations1, cc01, cc11, cc21, time1, ct1) ->
        let adjustedTime0       = float time0 / float iterations0
        let adjustedTime1       = float time1 / float iterations1

        let ratio = adjustedTime1 / adjustedTime0

        writeField (ratio.ToString (CultureInfo.InvariantCulture))

    writeNewLine ()

  let compareResults
    (name             : string            )
    (performanceRatio : float             )
    (data             : PerformanceData[] ) =
    infof "Comparing performance data between MINIJSON and %s" name

    dumper <| sprintf "---==> PERFORMANCE COMPARISON (MINIJSON - %s) <==---" name

    for testCase0, iterations0, cc00, cc10, cc20, time0, ct0 in miniJsonData do
      match data |> Array.tryFind (fun (testCase1, _, _, _, _, _, _) -> testCase0 = testCase1) with
      | None -> ()
      | Some (_, iterations1, cc01, cc11, cc21, time1, ct1) ->
        let adjustedTime0       = float time0 / float iterations0
        let adjustedTime1       = float time1 / float iterations1

        let adjustedCycleTime0  = float ct0   / float iterations0
        let adjustedCycleTime1  = float ct1   / float iterations1

        let ratio               = adjustedTime1       / adjustedTime0
        let ctratio             = adjustedCycleTime1  / adjustedCycleTime0

        dumper <| sprintf
          "TestCase: %s - iterations: %d, cc0: %d,%d, cc1: %d,%d, cc2: %d,%d, time: %d,%d ms, cycleTime: %d, %d"
          testCase0
          iterations1
          cc00  cc01
          cc10  cc11
          cc20  cc21
          time0 time1
          ct0   ct1

        check_lt (expectedRatio performanceRatio)   ratio         testCase0
        check_lt (expectedRatio performanceRatio)   ctratio       testCase0
        check_gt adjustedTime1                      adjustedTime0 testCase0

  writeResults "MINIJSON" miniJsonData

  let referenceData =
    let testCases =
      allTtestCases
      |> Array.filter filterForReference
      |> Array.filter filterForPerformance

    collectPerformanceData
      "REFERENCE"
      ReferenceJsonModule.parse
      200
      testCases
      dumper

  compareResults "REFERENCE" 3.5 referenceData

  let jilData =
    let testCases =
      allTtestCases
      |> Array.filter filterForJil
      |> Array.filter filterForPerformance

    collectPerformanceData
      "JIL"
      MiniJson.Tests.Jil.dummyParse
      1000
      testCases
      dumper

  compareResults "JIL" 1.15 jilData
  writeResults "JIL" jilData

  let jsonNetData =
    let testCases =
      allTtestCases
      |> Array.filter filterForJsonNet
      |> Array.filter filterForPerformance

    collectPerformanceData
      "JSON.NET"
      MiniJson.Tests.JsonNet.parse
      1000
      testCases
      dumper

  compareResults "JSON.NET" 1.1 jsonNetData
  writeResults "JSON.NET" jsonNetData

  let fsharpDataData =
    let testCases =
      allTtestCases
      |> Array.filter filterForFSharpData
      |> Array.filter filterForPerformance

    collectPerformanceData
      "FSHARP.DATA"
      MiniJson.Tests.FSharpData.dummyParse
      1000
      testCases
      dumper

  compareResults "FSHARP.DATA" 1.3 fsharpDataData
  writeResults "FSHARP.DATA" fsharpDataData
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
let errorReportingTestCases (dumper : string -> unit) =
  let testCases = Array.zip negativeTestCases errorReportingOracles

  infof "Running %d error reporting testcases..." testCases.Length

  for (positive, name, testCase), expected in testCases do
    dumper "---==> ERROR REPORTING <==---"
    dumper name
    dumper (if positive then "positive" else "negative")
    dumper testCase
    match parse true testCase with
    | Success v           -> test_failuref "Parsing expected to fail for '%s' : %A" name v
    | Failure (actual, _)  ->
      check_eq expected actual name
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
let toStringTestCases (dumper : string -> unit) =
  let testCases = Array.zip3 positiveTestCases noIndentOracles withIndentOracles

  infof "Running %d toString testcases..." testCases.Length

  for (positive, name, testCase), noIndentOracle ,withIndentOracle in testCases do
    dumper "---==> TO STRING <==---"
    dumper name
    dumper (if positive then "positive" else "negative")
    dumper testCase
    match parse false testCase with
    | Success v       ->
      check_eq noIndentOracle   (v.ToString ()    ) name
      check_eq noIndentOracle   (toString false v ) name
      if not linuxLineEnding then check_eq withIndentOracle (toString true v  ) name

    | Failure (_, _)  ->
      test_failuref "Parsing expected to succeed for '%s'" name
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
let scalarToStringTestCases (dumper : string -> unit) =
  infof "Running Scalar ToString testcases..."

  // Tests that toString on scalar values (like null) generate valid JSON documents

  let testCases =
    [|
      JsonNull            , """[null]"""
      JsonBoolean false   , """[false]"""
      JsonBoolean true    , """[true]"""
      JsonNumber  123.    , """[123]"""
      JsonString  "Hello" , """["Hello"]"""
    |]

  for testCase, expected in testCases do
    check_eq expected (toString false testCase) expected

// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
let pathTestCases (dumper : string -> unit) =
  infof "Running Dynamic JSON testcases..."

  let test_scalar e a =
    match e,a with
    | ScalarNull        _     , ScalarNull        _                 -> true
    | ScalarBoolean     (_,a) , ScalarBoolean     (_,b) when a = b  -> true
    | ScalarNumber      (_,a) , ScalarNumber      (_,b) when a = b  -> true
    | ScalarString      (_,a) , ScalarString      (_,b) when a = b  -> true
    | ScalarNotScalar   _     , ScalarNotScalar   _                 -> true
    | ScalarInvalidPath _     , ScalarInvalidPath _                 -> true
    | _                       , _                                   ->
      errorf "TEST_SCALAR: %A equiv %A" e a
      false

  let check_scalar e a = ignore <| test_scalar e a

  let jsonArray =
    JsonArray
      [|
        JsonNull
        JsonBoolean true
        JsonNumber  0.
        JsonString  "Hello"
      |]

  let jsonObject =
    JsonObject
      [|
        "Null"    , JsonNull
        "Boolean" , JsonBoolean false
        "Number"  , JsonNumber  123.
        "String"  , JsonString  "There"
        "Array"   , jsonArray
      |]

  let rootObject =
    JsonObject
      [|
        "Object"    , jsonObject
        "Array"     , jsonArray
      |]


  let defaultPath       = JsonNull, []
  let defaultNull       = ScalarNull defaultPath
  let defaultBoolean  v = ScalarBoolean (defaultPath, v)
  let defaultNumber   v = ScalarNumber  (defaultPath, v)
  let defaultString   v = ScalarString  (defaultPath, v)

  let path = rootObject.Query

  check_scalar (defaultNull           ) (!!path?Object?Null         )
  check_scalar (defaultBoolean false  ) (!!path?Object?Boolean      )
  check_scalar (defaultNumber  123.   ) (!!path?Object?Number       )
  check_scalar (defaultString  "There") (!!path?Object?String       )

  check_scalar (defaultNull           ) (!!path?Object?Array    .[0])
  check_scalar (defaultBoolean true   ) (!!path?Object?Array    .[1])
  check_scalar (defaultNumber  0.     ) (!!path?Object?Array    .[2])
  check_scalar (defaultString  "Hello") (!!path?Object?Array    .[3])

  check_scalar (defaultNull           ) (!!path?Array .[0]          )
  check_scalar (defaultBoolean true   ) (!!path?Array .[1]          )
  check_scalar (defaultNumber  0.     ) (!!path?Array .[2]          )
  check_scalar (defaultString  "Hello") (!!path?Array .[3]          )

  check_scalar (defaultNull           ) (!!path?Object.[0]          )
  check_scalar (defaultBoolean false  ) (!!path?Object.[1]          )
  check_scalar (defaultNumber  123.   ) (!!path?Object.[2]          )
  check_scalar (defaultString  "There") (!!path?Object.[3]          )

  check_scalar (defaultNull           ) (!!path?Array .Children.[0] )
  check_scalar (defaultBoolean true   ) (!!path?Array .Children.[1] )
  check_scalar (defaultNumber  0.     ) (!!path?Array .Children.[2] )
  check_scalar (defaultString  "Hello") (!!path?Array .Children.[3] )

  check_scalar (defaultNull           ) (!!path?Object.Children.[0] )
  check_scalar (defaultBoolean false  ) (!!path?Object.Children.[1] )
  check_scalar (defaultNumber  123.   ) (!!path?Object.Children.[2] )
  check_scalar (defaultString  "There") (!!path?Object.Children.[3] )

  check_eq false    path                .HasValue     "HasValue: path"
  check_eq false    path?Array          .HasValue     "HasValue: path?Array"
  check_eq false    path?Object         .HasValue     "HasValue: path?Object"
  check_eq false    path?Object?Null    .HasValue     "HasValue: path?Object?Null"
  check_eq true     path?Object?Boolean .HasValue     "HasValue: path?Object?Boolean"
  check_eq true     path?Object?Number  .HasValue     "HasValue: path?Object?Number"
  check_eq true     path?Object?String  .HasValue     "HasValue: path?Object?String"
  check_eq false    path?Object?Invalid .HasValue     "HasValue: path?Object?Invalid"
  check_eq false    path?Missing?Invalid.HasValue     "HasValue: path?Missing?Invalid"

  check_eq true     path                .Eval.IsError "IsError: path"
  check_eq true     path?Array          .Eval.IsError "IsError: path?Array"
  check_eq true     path?Object         .Eval.IsError "IsError: path?Object"
  check_eq false    path?Object?Null    .Eval.IsError "IsError: path?Object?Null"
  check_eq false    path?Object?Boolean .Eval.IsError "IsError: path?Object?Boolean"
  check_eq false    path?Object?Number  .Eval.IsError "IsError: path?Object?Number"
  check_eq false    path?Object?String  .Eval.IsError "IsError: path?Object?String"
  check_eq true     path?Object?Invalid .Eval.IsError "IsError: path?Object?Invalid"
  check_eq true     path?Missing?Invalid.Eval.IsError "IsError: path?Missing?Invalid"

  check_eq false    path                .AsBool       "AsBool: path"
  check_eq false    path?Array          .AsBool       "AsBool: path?Array"
  check_eq false    path?Object         .AsBool       "AsBool: path?Object"
  check_eq false    path?Object?Null    .AsBool       "AsBool: path?Object?Null"
  check_eq false    path?Object?Boolean .AsBool       "AsBool: path?Object?Boolean"
  check_eq true     path?Object?Number  .AsBool       "AsBool: path?Object?Number"
  check_eq true     path?Object?String  .AsBool       "AsBool: path?Object?String"
  check_eq false    path?Object?Invalid .AsBool       "AsBool: path?Object?Invalid"
  check_eq false    path?Missing?Invalid.AsBool       "AsBool: path?Missing?Invalid"

  check_eq 0.       path                .AsFloat      "AsFloat: path"
  check_eq 0.       path?Array          .AsFloat      "AsFloat: path?Array"
  check_eq 0.       path?Object         .AsFloat      "AsFloat: path?Object"
  check_eq 0.       path?Object?Null    .AsFloat      "AsFloat: path?Object?Null"
  check_eq 0.       path?Object?Boolean .AsFloat      "AsFloat: path?Object?Boolean"
  check_eq 123.     path?Object?Number  .AsFloat      "AsFloat: path?Object?Number"
  check_eq 0.       path?Object?String  .AsFloat      "AsFloat: path?Object?String"
  check_eq 0.       path?Object?Invalid .AsFloat      "AsFloat: path?Object?Invalid"
  check_eq 0.       path?Missing?Invalid.AsFloat      "AsFloat: path?Missing?Invalid"

  check_eq ""       path                .AsString     "AsString: path"
  check_eq ""       path?Array          .AsString     "AsString: path?Array"
  check_eq ""       path?Object         .AsString     "AsString: path?Object"
  check_eq ""       path?Object?Null    .AsString     "AsString: path?Object?Null"
  check_eq "false"  path?Object?Boolean .AsString     "AsString: path?Object?Boolean"
  check_eq "123"    path?Object?Number  .AsString     "AsString: path?Object?Number"
  check_eq "There"  path?Object?String  .AsString     "AsString: path?Object?String"
  check_eq ""       path?Object?Invalid .AsString     "AsString: path?Object?Invalid"
  check_eq ""       path?Missing?Invalid.AsString     "AsString: path?Missing?Invalid"

  let eRoot         = "NotScalar: root"
  let eRootA        = "NotScalar: root.Array"
  let eRootO        = "NotScalar: root.Object"
  let eInvalid      = "InvalidPath: root.Object!Invalid"
  let eMissing      = "InvalidPath: root!Missing!Invalid"

  check_eq eRoot    path                  .AsExpandedString     "AsExpandedString: path"
  check_eq eRootA   path?Array            .AsExpandedString     "AsExpandedString: path?Array"
  check_eq eRootO   path?Object           .AsExpandedString     "AsExpandedString: path?Object"
  check_eq "null"   path?Object?Null      .AsExpandedString     "AsExpandedString: path?Object?Null"
  check_eq "false"  path?Object?Boolean   .AsExpandedString     "AsExpandedString: path?Object?Boolean"
  check_eq "123"    path?Object?Number    .AsExpandedString     "AsExpandedString: path?Object?Number"
  check_eq "There"  path?Object?String    .AsExpandedString     "AsExpandedString: path?Object?String"
  check_eq eInvalid path?Object?Invalid   .AsExpandedString     "AsExpandedString: path?Object?Invalid"
  check_eq eMissing path?Missing?Invalid  .AsExpandedString     "AsExpandedString: path?Missing?Invalid"

  check_eq eRoot    (path                .Eval.ToString ())     "ToString: path"
  check_eq eRootA   (path?Array          .Eval.ToString ())     "ToString: path?Array"
  check_eq eRootO   (path?Object         .Eval.ToString ())     "ToString: path?Object"
  check_eq "null"   (path?Object?Null    .Eval.ToString ())     "ToString: path?Object?Null"
  check_eq "false"  (path?Object?Boolean .Eval.ToString ())     "ToString: path?Object?Boolean"
  check_eq "123"    (path?Object?Number  .Eval.ToString ())     "ToString: path?Object?Number"
  check_eq "There"  (path?Object?String  .Eval.ToString ())     "ToString: path?Object?String"
  check_eq eInvalid (path?Object?Invalid .Eval.ToString ())     "ToString: path?Object?Invalid"
  check_eq eMissing (path?Missing?Invalid.Eval.ToString ())     "ToString: path?Missing?Invalid"

  check_eq -1.      (path                 .ConvertToFloat -1.)  "ConvertToFloat: path"
  check_eq -1.      (path?Array           .ConvertToFloat -1.)  "ConvertToFloat: path?Array"
  check_eq -1.      (path?Object          .ConvertToFloat -1.)  "ConvertToFloat: path?Object"
  check_eq 0.       (path?Object?Null     .ConvertToFloat -1.)  "ConvertToFloat: path?Object?Null"
  check_eq 0.       (path?Object?Boolean  .ConvertToFloat -1.)  "ConvertToFloat: path?Object?Boolean"
  check_eq 123.     (path?Object?Number   .ConvertToFloat -1.)  "ConvertToFloat: path?Object?Number"
  check_eq -1.      (path?Object?String   .ConvertToFloat -1.)  "ConvertToFloat: path?Object?String"
  check_eq -1.      (path?Object?Invalid  .ConvertToFloat -1.)  "ConvertToFloat: path?Object?Invalid"
  check_eq -1.      (path?Missing?Invalid .ConvertToFloat -1.)  "ConvertToFloat: path?Missing?Invalid"

  check_eq "PathOk: root"                         (path                 .ToString ())           "ToString: path"
  check_eq "PathOk: root.Array"                   (path?Array           .ToString ())           "ToString: path?Array"
  check_eq "PathOk: root.Object"                  (path?Object          .ToString ())           "ToString: path?Object"
  check_eq "PathOk: root.Object.Null"             (path?Object?Null     .ToString ())           "ToString: path?Object?Null"
  check_eq "PathOk: root.Object.Boolean"          (path?Object?Boolean  .ToString ())           "ToString: path?Object?Boolean"
  check_eq "PathOk: root.Object.Number"           (path?Object?Number   .ToString ())           "ToString: path?Object?Number"
  check_eq "PathOk: root.Object.String"           (path?Object?String   .ToString ())           "ToString: path?Object?String"
  check_eq "PathError: root.Object!Invalid"       (path?Object?Invalid  .ToString ())           "ToString: path?Object?Invalid"
  check_eq "PathError: root!Missing!Invalid"      (path?Missing?Invalid .ToString ())           "ToString: path?Missing?Invalid"

  check_eq "InvalidPath: root![-1]"                 path.[-1]                 .AsExpandedString "AsExpandedString: path.[-1]"
  check_eq "InvalidPath: root.Array![-1]"           path?Array.[-1]           .AsExpandedString "AsExpandedString: path?Array.[-1]"
  check_eq "InvalidPath: root.Object![-1]"          path?Object.[-1]          .AsExpandedString "AsExpandedString: path?Object.[-1]"
  check_eq "InvalidPath: root.Object.Null![-1]"     path?Object?Null.[-1]     .AsExpandedString "AsExpandedString: path?Object?Null.[-1]"
  check_eq "InvalidPath: root.Object.Boolean![-1]"  path?Object?Boolean.[-1]  .AsExpandedString "AsExpandedString: path?Object?Boolean.[-1]"
  check_eq "InvalidPath: root.Object.Number![-1]"   path?Object?Number.[-1]   .AsExpandedString "AsExpandedString: path?Object?Number.[-1]"
  check_eq "InvalidPath: root.Object.String![-1]"   path?Object?String.[-1]   .AsExpandedString "AsExpandedString: path?Object?String.[-1]"
  check_eq "InvalidPath: root.Object!Invalid![-1]"  path?Object?Invalid.[-1]  .AsExpandedString "AsExpandedString: path?Object?Invalid.[-1]"
  check_eq "InvalidPath: root!Missing!Invalid![-1]" path?Missing?Invalid.[-1] .AsExpandedString "AsExpandedString: path?Missing?Invalid.[-1]"

  let eLongPath = "InvalidPath: root.Object.Array.[0]!Invalid![100]!Missing"
  let aLongPath = path?Object?Array.[0]?Invalid.[100]?Missing.AsExpandedString

  check_eq eLongPath aLongPath "Long path"

  let keys = [|"Null"; "Boolean"; "Number"; "String"; "Array"|]

  check_eq 4    path?Array          .Length           "Length: path?Array"
  check_eq 5    path?Object         .Length           "Length: path?Object"
  check_eq 0    path?Object?Null    .Length           "Length: path?Object?Null"
  check_eq 0    path?Object?Boolean .Length           "Length: path?Object?Boolean"
  check_eq 0    path?Object?Number  .Length           "Length: path?Object?Number"
  check_eq 0    path?Object?String  .Length           "Length: path?Object?String"
  check_eq 4    path?Object?Array   .Length           "Length: path?Object?Array"
  check_eq 0    path?Object?Invalid .Length           "Length: path?Object?Invalid"
  check_eq 0    path?Missing?Invalid.Length           "Length: path?Missing?Invalid"

  check_eq [||] path?Array          .Names            "Names: path?Array"
  check_eq keys path?Object         .Names            "Names: path?Object"
  check_eq [||] path?Object?Null    .Names            "Names: path?Object?Null"
  check_eq [||] path?Object?Boolean .Names            "Names: path?Object?Boolean"
  check_eq [||] path?Object?Number  .Names            "Names: path?Object?Number"
  check_eq [||] path?Object?String  .Names            "Names: path?Object?String"
  check_eq [||] path?Object?Array   .Names            "Names: path?Object?Array"
  check_eq [||] path?Object?Invalid .Names            "Names: path?Object?Invalid"
  check_eq [||] path?Missing?Invalid.Names            "Names: path?Missing?Invalid"

  check_eq 4    path?Array          .Children.Length  "Children: path?Array"
  check_eq 5    path?Object         .Children.Length  "Children: path?Object"
  check_eq 0    path?Object?Null    .Children.Length  "Children: path?Object?Null"
  check_eq 0    path?Object?Boolean .Children.Length  "Children: path?Object?Boolean"
  check_eq 0    path?Object?Number  .Children.Length  "Children: path?Object?Number"
  check_eq 0    path?Object?String  .Children.Length  "Children: path?Object?String"
  check_eq 4    path?Object?Array   .Children.Length  "Children: path?Object?Array"
  check_eq 0    path?Object?Invalid .Children.Length  "Children: path?Object?Invalid"
  check_eq 0    path?Missing?Invalid.Children.Length  "Children: path?Missing?Invalid"

// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
let adaptorTestCases (dumper : string -> unit) =
  infof "Running JSON C#/VB Adaptor testcases..."

  let positive = "[1,2,3]"
  let negative = "[1,2,]"

  let parseMessage = "No errors detected during parse"

  let jsonParser  = JsonParser (positive, false)
  check_eq false            (jsonParser.Equals null)        "Equals: null"
  check_eq true             (jsonParser.Equals jsonParser)  "Equals: jsonParser"
  check_eq 1342399656       (jsonParser.GetHashCode ())     "GetHashCode: jsonParser"
  check_eq 70               (jsonParser.ToString ()).Length "ToString: jsonParser"


  check_eq true             jsonParser.Success              "positive: Success"
  check_eq false            jsonParser.Failure              "positive: Failure"
  check_eq positive.Length  jsonParser.ParsePosition        "positive: ParsePosition"
  check_eq parseMessage     jsonParser.ParseMessage         "positive: ParsePosition"

  let path = makePath jsonParser.Result
  check_eq 3                path.Length                     "positive: path.Length"
  check_eq 1.               path.[0].AsFloat                "positive: path.[0].AsFloat"
  check_eq 2.               path.[1].AsFloat                "positive: path.[1].AsFloat"
  check_eq 3.               path.[2].AsFloat                "positive: path.[2].AsFloat"

  let ujson = "[1,2,3]"
  let ijson = """[
  1,
  2,
  3
]"""

  let dynamicResult = jsonParser.DynamicResult
  check_eq false          (dynamicResult.Equals null)                             "Equals: dynamicResult"
  check_eq true           (dynamicResult.Equals dynamicResult)                    "Equals: dynamicResult"
  check_eq -1988050296    (dynamicResult.GetHashCode ())                          "GetHashCode: dynamicResult"
  check_eq ""             (dynamicResult.ToString ())                             "ToString: dynamicResult"

  check_eq -1.            (dynamicResult.ConvertToFloat -1.)                      "positive: dynamicResult.ConvertToFloat -1."
  check_eq 3              (dynamicResult.GetChildren ()).Length                   "positive: dynamicResult.GetChildren ()"
  check_eq [||]           (dynamicResult.GetDynamicMemberNames () |> Seq.toArray) "positive: dynamicResult.GetDynamicMemberNames ()"
  check_eq 3              (dynamicResult.GetLength ())                            "positive: dynamicResult.GetLength ()"
  check_eq "PathOk: root" (dynamicResult.GetJsonPathString ())                    "positive: dynamicResult.GetJsonPathString ()"
  check_eq ujson          (dynamicResult.GetJsonString ())                        "positive: dynamicResult.GetJsonString ()"
  check_eq ujson          (dynamicResult.GetJsonString false)                     "positive: dynamicResult.GetJsonString false"
  check_eq ijson          (dynamicResult.GetJsonString true)                      "positive: dynamicResult.GetJsonString true"
  check_eq false          (dynamicResult.HasValue ())                             "positive: dynamicResult.HasValue ()"

  let parseMessage = """Failed to parse input as JSON
[1,2,
-----^ Pos: 5
Expected: '"', '-', '[', '{', digit, false, null or true
Unexpected: EOS"""

  try
    let jsonParser = JsonParser ("[1,2,", true)
    check_eq false            jsonParser.Success        "negative: Success"
    check_eq true             jsonParser.Failure        "negative: Failure"
    check_eq 5                jsonParser.ParsePosition  "negative: ParsePosition"
    check_eq parseMessage     jsonParser.ParseMessage   "negative: ParsePosition"

    ignore <| jsonParser.Result

    error "negative: Exception expected"
  with
  | JsonParseException (msg, pos) as x ->
    let jex = x :?> JsonParseException
    check_eq 5            pos               "negative: pos"
    check_eq parseMessage msg               "negative: msg"
    check_eq 5            jex.ErrorPosition "negative: ErrorPosition"
    check_eq parseMessage jex.ErrorMessage  "negative: ErrorMessage"
    check_eq parseMessage (jex.ToString ()) "negative: ToString ()"
  | ex ->
    errorf "jsonParser: Wrong Exception caught: %s" ex.Message

// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
[<EntryPoint>]
let main argv =
  try
    highlight "Starting JSON testcases..."
    infof "Running on mono: %A"   runningOnMono
    infof "Linux line ending: %A" linuxLineEnding

    Environment.CurrentDirectory <- AppDomain.CurrentDomain.BaseDirectory

#if !DUMP_JSON
    let dumper _            = ()
#else
    use dump = File.CreateText "dump.txt"
    let dumper (s : string) = dump.WriteLine s
#endif

    let testSuites =
      [|
        if not runningOnMono then yield functionalReferenceTestCases
//        functionalJilTestCases
        yield functionalJsonNetTestCases
        if not runningOnMono then yield functionalFSharpDataTestCases
        yield toStringTestCases
        if not linuxLineEnding then yield errorReportingTestCases
        yield scalarToStringTestCases
        yield pathTestCases
        yield adaptorTestCases
#if !DEBUG
        yield performanceTestCases
#endif
      |]

    for testSuite in testSuites do
      try
        testSuite dumper
      with
      | ex -> errorf "EXCEPTION: %s" ex.Message

  with
  | ex -> errorf "EXCEPTION: %s" ex.Message

  if errors = 0 then
    success "No errors detected"
    0
  else
    errorf "Detected %d error(s)" errors
    9999
// ----------------------------------------------------------------------------------------------
