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
open System.Text

open MiniJson
open MiniJson.JsonModule
open MiniJson.Tests.Test
open MiniJson.Tests.TestCases
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
let jsonAsString (random : Random) (json : Json) : string =
  let sb = StringBuilder ()

  let inline str (s : string)     = ignore <| sb.Append s
  let inline ch  (c : char)       = ignore <| sb.Append c
  let inline num (f : float)      = ignore <| sb.AppendFormat (CultureInfo.InvariantCulture, "{0}", f)
  let ws ()                       =
    let e = random.Next(-1,3)
    for i = 0 to e do
      match random.Next(0,6) with
      | 0 -> ch '\n'
      | 1 -> ch '\r'
      | 2 -> ch '\t'
      | _ -> ch ' '

  let estr (s : string) =
    ch '"'
    let e = s.Length - 1
    for i = 0 to e do
      match s.[i] with
      | '\"'  -> str @"\"""
      | '\\'  -> str @"\\"
      | '/'   -> str @"\/"
      | '\b'  -> str @"\b"
      | '\f'  -> str @"\f"
      | '\n'  -> str @"\n"
      | '\r'  -> str @"\r"
      | '\t'  -> str @"\t"
      | c     -> ch c
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
    ignore <| test_eq true  positive  name
    if test_eq e a testCase then postProcess e a
  | Failure (_,e) , Failure (_,a) ->
    ignore <| test_eq false positive  name
    ignore <| test_eq e     a         name
  | _             , _             ->
    test_failuref "Parsing mismatch '%s', expected:%A, actual: %A" name expected actual
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
let random = Random 19740531
let generatedTestCases = Array.init 1000 <| fun i ->
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
let functionalTestCases (dumper : string -> unit) =
  let testCases = Array.concat [|positiveTestCases; negativeTestCases; sampleTestCases; generatedTestCases|]

  infof "Running %d functional testcases..." testCases.Length

  runFunctionalTestCases
    "FUNCTIONAL TEST"
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
  | _ when name.StartsWith ("Negative: ") -> false  // JSON.NET is more relaxed when parsing therefore negative testcases can't be compared
  | _ -> true
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
let functionalJsonNetTestCases (dumper : string -> unit) =
  let testCases =
    Array.concat [|positiveTestCases; negativeTestCases; sampleTestCases; generatedTestCases |]
    |> Array.filter filterForJsonNet

  infof "Running %d functional testcases (JSON.NET)..." testCases.Length

  runFunctionalTestCases
    "JSON.NET TEST"
    (compareParsers MiniJson.Tests.JsonNet.parse)
    testCases
    dumper
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
let filterForPerformance (_,name : string ,tc : string) =
  match name with
  | _ when name.StartsWith ("Negative: ") -> false  // Negative test cases aren't tested for performance
  | _ -> tc.Length > 500
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
let runPerformanceTestCases
  (category       : string                  )
  (referenceParser: string -> ParseResult   )
  (testCases      : (bool*string*string) [] )
  (dumper         : string -> unit          ) : unit =
  let sw = Stopwatch ()

  let timeIt n (a : unit -> 'T) : int64*'T =
    sw.Reset ()

    let result = a ()

    sw.Start ()

    for i = 1 to n do
      ignore <| a ()

    sw.Stop ()

    sw.ElapsedMilliseconds, result

  for positive, name, testCase in testCases do
    dumper <| sprintf "---==> %s <==---" category
    dumper name
    dumper (if positive then "positive" else "negative")
    dumper testCase

    let iterations = 100

    let reference , _ = timeIt iterations (fun _ -> referenceParser testCase)
    let actual    , _ = timeIt iterations (fun _ -> parse false testCase)

    ignore <| test_eq true (reference > actual) name

    dumper <| sprintf "Iterations: %d, reference: %d ms, actual: %d ms" iterations reference actual
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
let performanceTestCases (dumper : string -> unit) =
  let testCases =
    Array.concat [|positiveTestCases; negativeTestCases; sampleTestCases; generatedTestCases |]
    |> Array.filter filterForPerformance

  infof "Running %d performance testcases..." testCases.Length

  runPerformanceTestCases
    "PERFORMANCE TEST"
    ReferenceJsonModule.parse
    testCases
    dumper
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
let performanceJsonNetTestCases (dumper : string -> unit) =
  let testCases =
    Array.concat [|positiveTestCases; negativeTestCases; sampleTestCases; generatedTestCases |]
    |> Array.filter filterForJsonNet
    |> Array.filter filterForPerformance

  infof "Running %d performance testcases (JSON.NET)..." testCases.Length

  runPerformanceTestCases
    "PERFORMANCE TEST (JSON.NET)"
    MiniJson.Tests.JsonNet.parse
    testCases
    dumper
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
let errorReportingTestCases (dumper : string -> unit) =
  let testCases = Array.concat [|negativeTestCases|]

  infof "Running %d error reporting testcases..." testCases.Length

  for positive, name, testCase in testCases do
    dumper "---==> ERROR REPORTING <==---"
    dumper name
    dumper (if positive then "positive" else "negative")
    dumper testCase
    match parse true testCase with
    | Success v           -> test_failuref "Parsing expected to fail for '%s' : %A" name v
    | Failure (msg, pos)  ->
      printfn "Pos: %d\n%s" pos msg
// ----------------------------------------------------------------------------------------------
[<EntryPoint>]
let main argv =
  try
    highlight "Starting JSON testcases..."

    Environment.CurrentDirectory <- AppDomain.CurrentDomain.BaseDirectory

#if !DUMP_JSON
    let dumper _            = ()
#else
    use dump = File.CreateText "dump.txt"
    let dumper (s : string) = dump.WriteLine s
#endif

    functionalTestCases         dumper
    functionalJsonNetTestCases  dumper
//  TODO: Figure out a good way to test error strings
//    errorReportingTestCases     dumper
#if !DEBUG
    performanceTestCases        dumper
//  TODO: Improve performance
//    performanceJsonNetTestCases dumper
#endif

  with
  | ex -> errorf "EXCEPTION: %s" ex.Message

  if errors = 0 then
    success "No errors detected"
    0
  else
    errorf "Detected %d error(s)" errors
    9999
// ----------------------------------------------------------------------------------------------
