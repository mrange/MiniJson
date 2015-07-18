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
let compareParsers (positive : bool) (testCase : string) (action : Json -> Json -> unit) : unit =
  let expected  = ReferenceJsonModule.parse testCase
  let actual    = parse false testCase

  match expected, actual with
  | Success e     , Success a     ->
    ignore <| test_eq true  positive  testCase
    if test_eq e a testCase then action e a
  | Failure (_,e) , Failure (_,a) ->
    ignore <| test_eq false positive  testCase
    ignore <| test_eq e     a         testCase
  | _             , _             ->
    test_failuref "Parsing mismatch '%s', expected:%A, actual: %A" testCase expected actual
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
let random = Random 19740531
let generatedTestCases = Array.init 1000 <| fun _ -> (randomizeJson 10 random |> fun json -> true, toString false json)
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
let functionalTestCases (dumper : string -> unit) =
  let testCases = Array.concat [|positiveTestCases; negativeTestCases; explicitTestCases; generatedTestCases|]

  let noAction _ _ = ()

  for positive, testCase in testCases do
    dumper "---==> FUNCTIONAL TEST <==---"
    dumper (if positive then "positive" else "negative")
    dumper testCase

    compareParsers positive testCase <| fun e a ->
      let unindented  = toString false e
      let indented    = toString true e
      let dumped      = jsonAsString random e

      dumper unindented
      dumper indented
      dumper dumped

      compareParsers positive unindented noAction
      compareParsers positive indented   noAction
      compareParsers positive dumped     noAction
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
let performanceTestCases (dumper : string -> unit) =
  let testCases = explicitTestCases |> Array.filter (fun (_, tc) -> tc.Length > 1000)

  let sw = Stopwatch ()

  let timeIt n (a : unit -> 'T) : int64*'T =
    sw.Reset ()

    let result = a ()

    sw.Start ()

    for i = 1 to n do
      ignore <| a ()

    sw.Stop ()

    sw.ElapsedMilliseconds, result

  for positive, testCase in testCases do
    dumper "---==> PERFORMANCE TEST <==---"
    dumper (if positive then "positive" else "negative")
    dumper testCase

    let iterations = 100

    let expected, _ = timeIt iterations (fun _ -> ReferenceJsonModule.parse testCase)    
    let actual  , _ = timeIt iterations (fun _ -> parse false testCase)    

    ignore <| test_eq true (expected > actual) testCase

    dumper <| sprintf "Iterations: %d, expected: %d ms, actual: %d ms" iterations expected actual
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
[<EntryPoint>]
let main argv =
  try
    highlight "Starting JSON testcases..."

    Environment.CurrentDirectory <- AppDomain.CurrentDomain.BaseDirectory

#if DUMP_JSON
    let dumper _            = ()
#else
    use dump = File.CreateText "dump.txt"
    let dumper (s : string) = dump.WriteLine s
#endif

    info "Running functional testcases..."
    functionalTestCases dumper
#if !DEBUG
    info "Running performance testcases..."
    performanceTestCases dumper
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
