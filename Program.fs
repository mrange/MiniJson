open System

open MiniJson.JsonModule

open Microsoft.FSharp.Text.Lexing

let parseAndPrint (input : string) : unit =
(*
  let lexbuf          = LexBuffer<char>.FromChars <| input.ToCharArray ()

  let json            = Parser.start Lexer.tokenstream lexbuf

  json
*)
  let r = parse input
  match r with
  | Success v -> 
    printfn "Parsed: %s\nPretty: %s\nAs    : %A" input (toString false v) v
  | Failure (desc, pos) ->
    printfn "Failed to parse : %s\n%d: %s" input pos desc
  

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
//  parseAndPrint """[null,true,false,"Test\r\n\/\\\u2665",0,-0,1,-1,123,-123,12.3,-1.23,12E3/*hello*/,-1.2E-3]"""
  parseAndPrint """[null]"""
  parseAndPrint """[true]"""
  parseAndPrint """[false]"""
  parseAndPrint """[false,true,null]"""
  parseAndPrint """ [ false ,true, null ] """
  parseAndPrint """[[], null, [true]]"""

[<EntryPoint>]
let main argv =
  //testSerializer ()
  testUnserializer ()

  0
