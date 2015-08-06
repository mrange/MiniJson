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

/// MiniJson aims to be a minimal yet conforming JSON parser with reasonable performance and decent error reporting
///   JSON Specification: http://json.org/
///   JSON Lint         : http://jsonlint.com/
///
/// MiniJson.JsonModule contains functionality to parse and serialize a JSON document
///
///
/// Example:
/// --------
///     let jsonText = """[{"id":"123", "name":"Mr. Big", "age":30}, {"id":"123", "name":"Mr. X"}]"""
///
///     match parse true jsonText with  // true for full error-info
///     | Failure (msg, pos)  -> printfn "Failure@%d\n%s" pos msg
///     | Success json        ->
///       printfn "Success\n%s" <| toString true json  // true to indent JSON
#if PUBLIC_MINIJSON
module MiniJson.JsonModule
#else
// Due to what seems to be an issue with the F# compiler preventing
//  access to internal operator ? from within the same assembly
//  define INTERNAL_MINIJSON_WORKAROUND to suppress internalizing of
//  MiniJson.
#if INTERNAL_MINIJSON_WORKAROUND
module Internal.MiniJson.JsonModule
#else
module internal Internal.MiniJson.JsonModule
#endif
#endif
open System
open System.Collections.Generic
open System.Diagnostics
open System.Globalization
open System.Text

module internal Tokens =
  [<Literal>]
  let Null              = "null"

  [<Literal>]
  let True              = "true"

  [<Literal>]
  let False             = "false"

  [<Literal>]
  let Char              = "char"

  [<Literal>]
  let Digit             = "digit"

  [<Literal>]
  let HexDigit          = "hexdigit"

  [<Literal>]
  let EOS               = "EOS"

  [<Literal>]
  let NewLine           = "NEWLINE"

  [<Literal>]
  let Escapes           = "\"\\/bfnrtu"

  [<Literal>]
  let ValuePreludes     = "\"{[-"

  [<Literal>]
  let RootValuePreludes = "{["

module internal ToStringDetails =
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

open ToStringDetails
/// Represents a JSON document
type Json =
  /// ()         - Represents a JSON null value
  | JsonNull
  /// (value)    - Represents a JSON boolean value
  | JsonBoolean of bool
  /// (value)    - Represents a JSON number value
  | JsonNumber  of float
  /// (value)    - Represents a JSON string value
  | JsonString  of string
  /// (values)   - Represents a JSON array value
  | JsonArray   of Json []
  /// (members)  - Represents a JSON object value
  | JsonObject  of (string*Json) []

  /// Converts a JSON document into a string
  ///   @doIndent  : True to indent
  member x.ToString (doIndent : bool) : string =
    let sb = StringBuilder ()

    let newline, indent, inc, dec =
      let doNothing () = ()
      if doIndent then
        let current = ref 0

        let newline ()  = ignore <| sb.AppendLine ()
        let indent ()   = ignore <| sb.Append (' ', !current)
        let inc ()      = current := !current + 2
        let dec ()      = current := !current - 2

        newline, indent, inc, dec
      else
        doNothing, doNothing, doNothing, doNothing

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

    let estr (s : string) =
      ch '"'
      let e = s.Length - 1
      for i = 0 to e do
        match s.[i] with
        | '\"'            -> str @"\"""
        | '\\'            -> str @"\\"
        | '/'             -> str @"\/"
        | c when c < ' '  -> str ToStringDetails.nonPrintableChars.[int c]
        | c               -> ch c
      ch '"'

    let values b e (vs : 'T array) (a : 'T -> unit) =
      ch b
      newline ()
      inc ()
      let ee = vs.Length - 1
      for i = 0 to ee do
        let v = vs.[i]
        indent ()
        a v
        if i < ee then
          ch ','
        newline ()
      dec ()
      indent ()
      ch e

    let rec impl j =
      match j with
      | JsonNull          -> str Tokens.Null
      | JsonBoolean true  -> str Tokens.True
      | JsonBoolean false -> str Tokens.False
      | JsonNumber n      -> num n
      | JsonString s      -> estr s
      | JsonArray vs      -> values '[' ']' vs impl
      | JsonObject ms     -> values '{' '}' ms implkv
    and implkv (k,v) =
      estr k
      ch ':'
      newline ()
      inc ()
      indent ()
      impl v
      dec ()

    let json =
      match x with
      | JsonNull
      | JsonBoolean _
      | JsonNumber  _
      | JsonString  _ -> JsonArray [|x|]  // In order to be valid JSON
      | JsonArray   _
      | JsonObject  _ -> x

    impl json

    sb.ToString ()

  /// Converts a JSON document into a string
  override x.ToString () : string =
    x.ToString false

/// Converts a JSON document into a string
///   @doIndent  : True to indent
///   @json      : The JSON document
let toString doIndent (json : Json) : string =
  json.ToString doIndent

/// IParseVisitor is implemented by users wanting to parse
///   a JSON document into a data structure other than MiniJson.Json
type IParseVisitor =
  interface
    abstract NullValue    : unit          -> bool
    abstract BoolValue    : bool          -> bool
    abstract NumberValue  : float         -> bool
    abstract StringValue  : StringBuilder -> bool
    abstract ArrayBegin   : unit          -> bool
    abstract ArrayEnd     : unit          -> bool
    abstract ObjectBegin  : unit          -> bool
    abstract ObjectEnd    : unit          -> bool
    abstract MemberKey    : StringBuilder -> bool
    abstract ExpectedChar : int*char      -> unit
    abstract Expected     : int*string    -> unit
    abstract Unexpected   : int*string    -> unit
  end

module internal ParserDetails =
  [<Literal>]
  let DefaultSize = 16

  [<Literal>]
  let ErrorPrelude = "Failed to parse input as JSON"

  // Min & Max Exponent of float (double)
  //  https://en.wikipedia.org/wiki/Double-precision_floating-point_format

  [<Literal>]
  let MinimumPow10  = -323  // Min Exponent is -1022 (binary) but since double supports subnormals effective is even lower

  [<Literal>]
  let MaximumPow10  = 308   // 1023 (binary)

  let Pow10Table =
    [|
      for i in MinimumPow10..MaximumPow10 -> pown 10. i
    |]

  let pow10 (n : int) : float =
    if n < MinimumPow10 then 0.
    elif n > MaximumPow10 then Double.PositiveInfinity
    else Pow10Table.[n - MinimumPow10]

  let inline isWhiteSpace (c : char) : bool =
    c = ' ' || c = '\t' || c = '\n' || c = '\r'

  let inline isDigit (c : char) : bool =
    c >= '0' && c <= '9'

  type IParseVisitor with
    member x.ExpectedChars (p : int, chars : string) : unit =
      let e = chars.Length - 1
      for i = 0 to e do
        x.ExpectedChar (p, chars.[i])

  type JsonParser(s : string, v : IParseVisitor) =
    let sb          = StringBuilder DefaultSize
    let mutable pos = 0

    member x.position           :int   = pos

// COVERAGE: inline makes code coverage not work
#if DEBUG
    member x.neos        : bool = pos < s.Length
    member x.eos         : bool = pos >= s.Length
    member x.ch          : char = s.[pos]
    member x.adv ()      : unit = pos <- pos + 1
#else
    member inline x.neos        : bool = pos < s.Length
    member inline x.eos         : bool = pos >= s.Length
    member inline x.ch          : char = s.[pos]
    member inline x.adv ()      : unit = pos <- pos + 1
#endif

    member x.raise_Eos ()       : bool =
      v.Unexpected (pos, Tokens.EOS)
      false

    member x.raise_Value ()     : bool =
      v.Expected      (pos, Tokens.Null )
      v.Expected      (pos, Tokens.True )
      v.Expected      (pos, Tokens.False)
      v.Expected      (pos, Tokens.Digit)
      v.ExpectedChars (pos, Tokens.ValuePreludes)
      false

    member x.raise_RootValue () : bool =
      v.ExpectedChars (pos, Tokens.RootValuePreludes)
      false

    member x.raise_Char () : bool =
      v.Expected (pos, Tokens.Char)
      false

    member x.raise_Digit () : bool =
      v.Expected (pos, Tokens.Digit)
      false

    member x.raise_HexDigit () : bool =
      v.Expected (pos, Tokens.HexDigit)
      false

    member x.raise_Escapes () : bool =
      v.ExpectedChars (pos, Tokens.Escapes)
      false

// COVERAGE: inline makes code coverage not work
#if DEBUG
    member x.consume_WhiteSpace () : bool =
#else
    member inline x.consume_WhiteSpace () : bool =
#endif
      let l = s.Length
      while pos < l && (isWhiteSpace x.ch) do
        x.adv ()
      true

// COVERAGE: inline makes code coverage not work
#if DEBUG
    member x.test_Char (c : char) : bool =
#else
    member inline x.test_Char (c : char) : bool =
#endif
      x.neos && x.ch  = c

// COVERAGE: inline makes code coverage not work
#if DEBUG
    member x.tryConsume_Char (c : char) : bool =
#else
    member inline x.tryConsume_Char (c : char) : bool =
#endif
      if x.eos then
        v.ExpectedChar (pos, c)
        x.raise_Eos ()
      elif x.ch = c then
        x.adv ()
        true
      else
        v.ExpectedChar (pos, c)
        false

// inline causes DEBUG mode to crash (because F# creates tuples of pointers)
// COVERAGE: inline makes code coverage not work
#if DEBUG
    member x.tryParse_AnyOf2 (first : char, second : char, r : char byref) : bool =
#else
    member inline x.tryParse_AnyOf2 (first : char, second : char, r : char byref) : bool =
#endif
      if x.eos then
        v.ExpectedChar (pos, first)
        v.ExpectedChar (pos, second)
        x.raise_Eos ()
      else
        let c = x.ch
        if c = first || c = second then
          r <- c
          x.adv ()
          true
        else
          v.ExpectedChar (pos, first)
          v.ExpectedChar (pos, second)
          false

// COVERAGE: inline makes code coverage not work
#if DEBUG
    member x.tryConsume_Token (tk : string) : bool =
#else
    member inline x.tryConsume_Token (tk : string) : bool =
#endif
      let tkl           = tk.Length

      if tkl + pos <= s.Length then
        let spos          = pos
        let mutable tpos  = 0

        while tpos < tkl && tk.[tpos] = x.ch do
          tpos <- tpos + 1
          x.adv ()

        if tpos = tkl then true
        else
          // To support error reporting, move back on failure
          pos <- spos
          false
      else
        false

    member x.tryParse_Null () : bool =
      if x.tryConsume_Token Tokens.Null then
        v.NullValue ()
      else
        x.raise_Value ()

    member x.tryParse_True () : bool =
      if x.tryConsume_Token Tokens.True then
        v.BoolValue true
      else
        x.raise_Value ()

    member x.tryParse_False () : bool =
      if x.tryConsume_Token Tokens.False then
        v.BoolValue false
      else
        x.raise_Value ()

    member x.tryParse_UInt (first : bool, r : float byref) : bool =
      let z = float '0'
      if x.eos then x.raise_Digit () || x.raise_Eos () || not first
      else
        let c = x.ch
        if c >= '0' && c <= '9' then
          x.adv ()
          r <- 10.0*r + (float c - z)
          x.tryParse_UInt (false, &r)
        else
          x.raise_Digit () || not first

    member x.tryParse_UInt0 (r : float byref) : bool =
      // tryParse_UInt0 only consumes 0 if input is 0123, this in order to be conformant with spec
      let zero = x.tryConsume_Char '0'

      if zero then
        r <- 0.0
        true
      else
        x.tryParse_UInt (true, &r)

    member x.tryParse_Fraction (r : float byref) : bool =
      if x.tryConsume_Char '.' then
        let spos        = pos
        let mutable uf  = 0.0
        if x.tryParse_UInt (true, &uf) then
          r <- (float uf) * (pow10 (spos - pos))
          true
        else
          false
      else
        true  // Fraction is optional

    member x.tryParse_Exponent (r : int byref) : bool =
      let mutable exp = ' '
      if x.tryParse_AnyOf2 ('e', 'E', &exp) then
        let mutable sign = '+'
        // Ignore as sign is optional
        ignore <| x.tryParse_AnyOf2 ('+', '-', &sign)
        // TODO: Parsing exponent as float seems unnecessary
        let mutable ue = 0.0
        if x.tryParse_UInt (true, &ue) then
          let ur = int ue
          r <- if sign = '-' then -ur else ur
          true
        else
          false
      else
        true  // Fraction is optional

    member x.tryParse_Number () : bool =
      let hasSign       = x.tryConsume_Char '-'

      let mutable i = 0.0
      let mutable f = 0.0
      let mutable e = 0

      let result =
        x.tryParse_UInt0        (&i)
        && x.tryParse_Fraction  (&f)
        && x.tryParse_Exponent  (&e)

      if result then
        let uu = i + f
        let ff = if hasSign then -uu else uu
        let ee = pow10 e
        let rr = ff*ee
        v.NumberValue rr
      else
        false

    member x.tryParse_UnicodeChar (n : int, r : int) : bool =
      if n = 0 then
        ignore <| sb.Append (char r)
        true
      elif x.eos then x.raise_HexDigit () || x.raise_Eos ()
      else
        let sr  = r <<< 4
        let   c = x.ch
        if    c >= '0' && c <= '9'  then x.adv () ; x.tryParse_UnicodeChar (n - 1, sr + (int c - int '0'))
        elif  c >= 'A' && c <= 'F'  then x.adv () ; x.tryParse_UnicodeChar (n - 1, sr + (int c - int 'A' + 10))
        elif  c >= 'a' && c <= 'f'  then x.adv () ; x.tryParse_UnicodeChar (n - 1, sr + (int c - int 'a' + 10))
        else
          x.raise_HexDigit ()

    member x.tryParse_Chars (b : int) : bool =
      let inline app (c : char) = ignore <| sb.Append c
      let inline seq (e :int)   = ignore <| sb.Append (s, b, e - b)

      if x.eos then x.raise_Char () || x.raise_Eos ()
      else
        let c = x.ch
        match c with
        | '"'         -> seq pos; true
        | '\r' | '\n' -> v.Unexpected (pos, Tokens.NewLine); false
        | '\\'        ->
          seq pos
          x.adv ()
          if x.eos then x.raise_Escapes () || x.raise_Eos ()
          else
            let e = x.ch
            let result =
              match e with
              | '"'
              | '\\'
              | '/' -> app e    ; x.adv (); true
              | 'b' -> app '\b' ; x.adv (); true
              | 'f' -> app '\f' ; x.adv (); true
              | 'n' -> app '\n' ; x.adv (); true
              | 'r' -> app '\r' ; x.adv (); true
              | 't' -> app '\t' ; x.adv (); true
              | 'u' ->
                x.adv ()
                x.tryParse_UnicodeChar (4, 0)
              | _ -> x.raise_Escapes ()
            result && x.tryParse_Chars pos
        | _           ->
          x.adv ()
          x.tryParse_Chars b

    member x.tryParse_ToStringBuilder () : bool =
      ignore <| sb.Clear ()
      x.tryConsume_Char     '"'
      && x.tryParse_Chars   pos
      && x.tryConsume_Char  '"'

    member x.tryParse_String () : bool =
      x.tryParse_ToStringBuilder ()
      && v.StringValue sb

    member x.tryParse_MemberKey () : bool =
      x.tryParse_ToStringBuilder ()
      && v.MemberKey sb

// COVERAGE: inline makes code coverage not work
#if DEBUG
    member x.tryConsume_Delimiter (first : bool) : bool =
#else
    member inline x.tryConsume_Delimiter (first : bool) : bool =
#endif
      if first then true
      else
        x.tryConsume_Char       ','
        && x.consume_WhiteSpace ()

    member x.tryParse_ArrayValues (first : bool) : bool =
      if x.test_Char ']' then
        true
      else
        x.tryConsume_Delimiter    first
        && x.tryParse_Value       ()
        && x.tryParse_ArrayValues false

    member x.tryParse_Array () : bool =
      x.tryConsume_Char         '['
      && x.consume_WhiteSpace   ()
      && v.ArrayBegin           ()
      && x.tryParse_ArrayValues true
      && x.tryConsume_Char      ']'
      && v.ArrayEnd             ()

    member x.tryParse_ObjectMembers (first : bool) : bool =
      if x.test_Char '}' then
        true
      else
        x.tryConsume_Delimiter      first
        && x.tryParse_MemberKey     ()
        && x.consume_WhiteSpace     ()
        && x.tryConsume_Char        ':'
        && x.consume_WhiteSpace     ()
        && x.tryParse_Value         ()
        && x.tryParse_ObjectMembers false

    member x.tryParse_Object () : bool =
      x.tryConsume_Char           '{'
      && x.consume_WhiteSpace     ()
      && v.ObjectBegin            ()
      && x.tryParse_ObjectMembers true
      && x.tryConsume_Char        '}'
      && v.ObjectEnd              ()

    member x.tryParse_Value (): bool =
      if x.eos then x.raise_Value () || x.raise_Eos ()
      else
        let result =
          match x.ch with
          | 'n'                 -> x.tryParse_Null    ()
          | 't'                 -> x.tryParse_True    ()
          | 'f'                 -> x.tryParse_False   ()
          | '['                 -> x.tryParse_Array   ()
          | '{'                 -> x.tryParse_Object  ()
          | '"'                 -> x.tryParse_String  ()
          | '-'                 -> x.tryParse_Number  ()
          | c when isDigit c    -> x.tryParse_Number  ()
          | _                   -> x.raise_Value      ()
        result && x.consume_WhiteSpace ()

    member x.tryParse_RootValue () : bool =
      if x.eos then x.raise_RootValue () || x.raise_Eos ()
      else
        let result =
          match x.ch with
          | '['                 -> x.tryParse_Array  ()
          | '{'                 -> x.tryParse_Object ()
          | _                   -> x.raise_RootValue ()
        result && x.consume_WhiteSpace ()

    member x.tryParse_Eos () : bool =
      if x.eos then
        true
      else
        v.Expected (pos, Tokens.EOS)
        false

  [<AbstractClass>]
  [<NoEquality>]
  [<NoComparison>]
  type BaseJsonValueBuilder() =
    abstract AddValue     : Json                                                    -> bool
    abstract SetKey       : string                                                  -> bool
    abstract CreateValue  : Stack<BaseJsonValueBuilder>*Stack<BaseJsonValueBuilder> -> Json

  let emptyString         = ""
  let nullValue           = JsonNull
  let trueValue           = JsonBoolean true
  let falseValue          = JsonBoolean false
  let inline boolValue b  = if b then trueValue else falseValue

  [<Sealed>]
  [<NoEquality>]
  [<NoComparison>]
  type RootJsonValueBuilder() =
    inherit BaseJsonValueBuilder()

    let mutable root = nullValue

    override x.AddValue (json : Json) : bool =
      root <- json
      true
    override x.SetKey (key : string) : bool =
      Debug.Assert false
      true
    override x.CreateValue (freeArrayBuilders, freeObjectBuilders) : Json =
      let result = root
      root <- nullValue
      result

  [<Sealed>]
  [<NoEquality>]
  [<NoComparison>]
  type ArrayJsonValueBuilder() =
    inherit BaseJsonValueBuilder()

    let mutable values = ResizeArray<Json> DefaultSize

    override x.AddValue (json : Json) : bool =
      values.Add json
      true
    override x.SetKey (key : string) : bool =
      Debug.Assert false
      true
    override x.CreateValue (freeArrayBuilders, freeObjectBuilders) : Json =
      let result = JsonArray (values.ToArray ())
      values.Clear ()
      freeArrayBuilders.Push x
      result

  [<Sealed>]
  type ObjectJsonValueBuilder() =
    inherit BaseJsonValueBuilder()

    let mutable key     = emptyString
    let mutable members = ResizeArray<string*Json> DefaultSize

    override x.AddValue (json : Json) : bool =
      members.Add (key, json)
      true
    override x.SetKey (k : string) : bool =
      key <- k
      true
    override x.CreateValue (freeArrayBuilders, freeObjectBuilders) : Json =
      let result = JsonObject (members.ToArray ())
      key <- emptyString
      members.Clear ()
      freeObjectBuilders.Push x
      result

  let inline setKey (context : Stack<BaseJsonValueBuilder>) (key : string) : bool =
    let v = context.Peek ()
    v.SetKey key

  let inline addValue (context : Stack<BaseJsonValueBuilder>) (json : Json) : bool =
    let v = context.Peek ()
    v.AddValue json

  let inline popContext (context : Stack<BaseJsonValueBuilder>) freeArrayBuilders freeObjectBuilders : Json =
    let v = context.Pop ()
    v.CreateValue (freeArrayBuilders, freeObjectBuilders)

  [<Sealed>]
  [<NoEquality>]
  [<NoComparison>]
  type JsonParseVisitor() =
    let context             = Stack<BaseJsonValueBuilder> DefaultSize
    let freeArrayBuilders   = Stack<BaseJsonValueBuilder> DefaultSize
    let freeObjectBuilders  = Stack<BaseJsonValueBuilder> DefaultSize

    do
      context.Push (RootJsonValueBuilder ())

    interface IParseVisitor with
      override x.NullValue    ()      = addValue context  <| nullValue
      override x.BoolValue    v       = addValue context  <| boolValue v
      override x.NumberValue  v       = addValue context  <| JsonNumber v
      override x.StringValue  v       = addValue context  <| JsonString (v.ToString ())

      override x.ArrayBegin   ()      =
        if freeArrayBuilders.Count > 0 then
          context.Push (freeArrayBuilders.Pop ())
        else
          context.Push (ArrayJsonValueBuilder ())
        true
      override x.ArrayEnd     ()      = addValue context  <| popContext context freeArrayBuilders freeObjectBuilders
      override x.ObjectBegin  ()      =
        if freeObjectBuilders.Count > 0 then
          context.Push (freeObjectBuilders.Pop ())
        else
          context.Push (ObjectJsonValueBuilder ())
        true
      override x.ObjectEnd    ()      = addValue context  <| popContext context freeArrayBuilders freeObjectBuilders
      override x.MemberKey    v       = setKey context    <| v.ToString ()

      override x.ExpectedChar (p,e)   = ()
      override x.Expected     (p,e)   = ()
      override x.Unexpected   (p,ue)  = ()

    member x.Root ()  =
      Debug.Assert (context.Count = 1)
      popContext context freeArrayBuilders freeObjectBuilders

  [<Sealed>]
  [<NoEquality>]
  [<NoComparison>]
  type JsonErrorParseVisitor(epos : int) =
    let expectedChars = ResizeArray<char>   DefaultSize
    let expected      = ResizeArray<string> DefaultSize
    let unexpected    = ResizeArray<string> DefaultSize

    let filter f      = f |> Seq.sort |> Seq.distinct |> Seq.toArray

    interface IParseVisitor with
      override x.NullValue    ()      = true
      override x.BoolValue    v       = true
      override x.NumberValue  v       = true
      override x.StringValue  v       = true

      override x.ArrayBegin   ()      = true
      override x.ArrayEnd     ()      = true
      override x.ObjectBegin  ()      = true
      override x.ObjectEnd    ()      = true
      override x.MemberKey    v       = true

      override x.ExpectedChar (p,e)   = if p = epos then expectedChars.Add e
      override x.Expected     (p,e)   = if p = epos then expected.Add e
      override x.Unexpected   (p,ue)  = if p = epos then unexpected.Add ue

    member x.ExpectedChars  = filter expectedChars
    member x.Expected       = filter expected
    member x.Unexpected     = filter unexpected

open ParserDetails

/// Attempts to parse a JSON document from a string
///   visitor : Parser visitor object
///   input   : Input string
let tryParse (visitor : IParseVisitor) (input : string) (pos : int byref) : bool =
  let jp = JsonParser (input, visitor)
  let result =
    jp.consume_WhiteSpace     ()
    && jp.tryParse_RootValue  ()
    && jp.tryParse_Eos        ()

  pos <- jp.position

  result

/// Returned by parse function
type ParseResult =
  /// (json) - Holds the parsed JSON document
  | Success of Json
  /// (message, pos) - Holds the error description and position of failure
  | Failure of string*int

/// Attempts to parse a JSON document from a string
///   @fullErrorInfo : True to generate full errorinfo
///                   False only shows position (faster)
///   @input         : Input string
let parse (fullErrorInfo : bool) (input : string) : ParseResult =
  let mutable pos = 0
  let v           = JsonParseVisitor ()

  match tryParse (upcast v) input &pos, fullErrorInfo with
  | true  , _     ->
    Success (v.Root ())
  | false , false ->
    Failure (ErrorPrelude, pos)
  | false , true  ->
    let mutable epos  = 0
    let ev            = JsonErrorParseVisitor (pos)

    ignore <| tryParse (upcast ev) input &epos

    let sb = StringBuilder ()
    let inline str  (s : string)  = ignore <| sb.Append s
    let inline strl (s : string)  = ignore <| sb.AppendLine s
    let inline ch   (c : char)    = ignore <| sb.Append c
    let inline line ()            = ignore <| sb.AppendLine ()

    let e =
      Seq.concat
        [|
          ev.ExpectedChars  |> Seq.map (fun c -> "'" + (c.ToString ()) + "'")
          upcast ev.Expected
        |]
      |> Seq.toArray
    let ue = ev.Unexpected

    let values prefix (vs : string []) =
      if vs.Length = 0 then ()
      else
        line ()
        str prefix
        let e = vs.Length - 1
        for i = 0 to e do
          let v = vs.[i]
          if i = 0 then ()
          elif i = e then str " or "
          else str ", "
          str v

    let windowSize = 60
    let windowBegin,windowEnd,windowPos =
      if input.Length < windowSize then
        0, input.Length - 1, pos
      else
        let hs  = windowSize / 2
        let b   = pos - hs
        let e   = pos + hs
        let ab  = max 0 b
        let ae  = min (input.Length - 1) (e + ab - b)
        let ap  = pos - ab
        ab, ae, ap

    strl ErrorPrelude
    for i = windowBegin to windowEnd do
      let c =
        match input.[i] with
        | '\n'
        | '\r'  -> ' '
        | c     -> c
      ch c
    line ()
    ignore <| sb.Append ('-', windowPos)
    str "^ Pos: "
    ignore <| sb.Append pos
    values "Expected: " e
    values "Unexpected: " ue

    Failure (sb.ToString (), pos)
