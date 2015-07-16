module MiniJson.JsonModule

open System
open System.Collections.Generic
open System.Diagnostics
open System.Globalization
open System.Text

type Json =
  | JsonNull
  | JsonBoolean of bool
  | JsonNumber  of float
  | JsonString  of string
  | JsonArray   of Json list
  | JsonObject  of (string*Json) list

let toString (doIndent : bool) (json : Json) : string =
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

  let inline str (s : string) = ignore <| sb.Append s
  let inline ch  (c : char)   = ignore <| sb.Append c
  let inline num (f : float)  = ignore <| sb.AppendFormat (CultureInfo.InvariantCulture, "{0}", f)

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

  let rec loop (vs : 'T list) (a : 'T -> unit) =
    match vs with
    | []    -> ()
    | v::vv -> 
      indent ()
      a v
      if not vv.IsEmpty then
        ch ','
      newline ()
      loop vv a // TODO: Check tail recursive

  let values b e (vs : 'T list) (a : 'T -> unit) = 
    ch b
    newline ()
    inc ()
    loop vs a
    dec ()
    indent ()
    ch e

  let rec impl j = 
    match j with
    | JsonNull          -> str "null"
    | JsonBoolean true  -> str "true"
    | JsonBoolean false -> str "false"
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

  impl json

  sb.ToString ()

type ParseVisitor =
  interface
    abstract NullValue    : unit        -> bool
    abstract BoolValue    : bool        -> bool
    abstract NumberValue  : double      -> bool
    abstract StringValue  : string      -> bool
    abstract ArrayBegin   : unit        -> bool
    abstract ArrayEnd     : unit        -> bool
    abstract ObjectBegin  : unit        -> bool
    abstract ObjectEnd    : unit        -> bool
    abstract MemberKey    : string      -> bool
    abstract ExpectedChar : int*char    -> unit
    abstract Expected     : int*string  -> unit
    abstract Unexpected   : int*string  -> unit
  end

type ParseResult =
  | Success of Json
  | Failure of string*int

module Details =

  let inline eos (s : string) (pos : int) : bool =
    not (pos < s.Length)

  let inline ch (s : string) (pos : int) : char =
    s.[pos]

  let inline raiseEos (v : ParseVisitor) (pos : int) : bool =
    v.Unexpected (pos, "EOS")
    false

  let raiseValue (v : ParseVisitor) (pos : int) : bool =
    v.Expected      (pos, "null"  )
    v.Expected      (pos, "true"  )
    v.Expected      (pos, "false" )
    v.Expected      (pos, "STRING")
    v.ExpectedChar  (pos, '{'     )
    v.ExpectedChar  (pos, '['     )
    v.ExpectedChar  (pos, '-'     )
    v.Expected      (pos, "digit" )
    false
    
  let inline isWhiteSpace (c : char) : bool =
    match c with
    | '\t'
    | '\n'
    | '\r'
    | ' ' -> true
    | _   -> false

  let inline consume_WhiteSpace (s : string) (pos : byref<int>) : bool =
    let l = s.Length
    while pos < l && (isWhiteSpace s.[pos]) do
      pos <- pos + 1
    true

  let inline isDigit (c : char) : bool =
    match c with
    | '0'
    | '1'
    | '2'
    | '3'
    | '4'
    | '5'
    | '6'
    | '7'
    | '8'
    | '9' -> true
    | _   -> false

  let inline isDigit19 (c : char) : bool =
    match c with
    | '1'
    | '2'
    | '3'
    | '4'
    | '5'
    | '6'
    | '7'
    | '8'
    | '9' -> true
    | _   -> false

  [<Literal>]
  let token_Null  = "null"

  [<Literal>]
  let token_True  = "true"

  [<Literal>]
  let token_False = "false"

  let inline test_Char (c : char) (s : string) (pos : int) : bool =
    if eos s pos then false
    else ch s pos = c

  let inline tryConsume_Char (v : ParseVisitor) (c : char) (s : string) (pos : byref<int>) : bool =
    if eos s pos then false
    elif (ch s pos) = c then
      pos <- pos + 1
      true
    else
      v.ExpectedChar (pos, c)
      false

  let inline tryConsume_Token (tk : string) (s : string) (pos : byref<int>) : bool =
    let tkl = tk.Length
    if pos + tkl <= s.Length then
      let mutable tpos = 0

      while tpos < tkl && tk.[tpos] = s.[pos] do
        tpos  <- tpos + 1
        pos   <- pos + 1

      tpos = tkl
    else
      // TODO: raiseEos?
      false

  let tryParse_Null (v : ParseVisitor) (s : string) (pos : byref<int>) : bool =
    let spos = pos
    if tryConsume_Token token_Null s &pos then
      v.NullValue ()
    else
      raiseValue v spos

  let tryParse_True (v : ParseVisitor) (s : string) (pos : byref<int>) : bool =
    let spos = pos
    if tryConsume_Token token_True s &pos then
      v.BoolValue true
    else
      raiseValue v spos

  let tryParse_False (v : ParseVisitor) (s : string) (pos : byref<int>) : bool =
    let spos = pos
    if tryConsume_Token token_False s &pos then
      v.BoolValue false
    else
      raiseValue v spos

  let tryParse_Number (v : ParseVisitor) (s : string) (pos : byref<int>) : bool =
    false

  let tryParse_ToStringBuilder (sb : StringBuilder) (s : string) (pos : byref<int>) : bool =
    ignore <| sb.Clear ()
    false

  let tryParse_String (v : ParseVisitor) (s : string) (pos : byref<int>) : bool =
    let spos = pos
    let sb = StringBuilder ()
    let result = tryParse_ToStringBuilder sb s &pos
    if result then
      v.StringValue (sb.ToString ())
    else 
      raiseValue v spos

  let tryParse_MemberKey (v : ParseVisitor) (s : string) (pos : byref<int>) : bool =
    // TODO: Reuse StringBuilder
    let spos = pos
    let sb = StringBuilder ()
    let result = tryParse_ToStringBuilder sb s &pos
    if result then
      v.MemberKey (sb.ToString ())
    else 
      v.Expected (spos, "STRING")
      false

  let inline tryConsume_Delimiter first (v : ParseVisitor) (s : string) (pos : byref<int>) : bool =
    if first then true
    else
      tryConsume_Char         v ',' s &pos
      && consume_WhiteSpace         s &pos

  let rec tryParse_ArrayValues first (v : ParseVisitor) (s : string) (pos : byref<int>) : bool =
    if test_Char ']' s pos then
      true
    else
      tryConsume_Delimiter    first v s &pos 
      && tryParse_Value             v s &pos
      && tryParse_ArrayValues false v s &pos      // TODO: Check this is tail recursive

  and tryParse_Array (v : ParseVisitor) (s : string) (pos : byref<int>) : bool =
    tryConsume_Char           v '[' s &pos
    && consume_WhiteSpace           s &pos
    && v.ArrayBegin ()
    && tryParse_ArrayValues true  v s &pos
    && tryConsume_Char        v ']' s &pos
    && v.ArrayEnd ()
  
  and tryParse_ObjectMembers first (v : ParseVisitor) (s : string) (pos : byref<int>) : bool =
    if test_Char '}' s pos then
      true
    else
      tryConsume_Delimiter      first v s &pos 
      && tryParse_MemberKey           v s &pos
      && consume_WhiteSpace             s &pos
      && tryConsume_Char          v ':' s &pos
      && consume_WhiteSpace             s &pos
      && tryParse_Value               v s &pos
      && tryParse_ObjectMembers false v s &pos      // TODO: Check this is tail recursive

  and tryParse_Object (v : ParseVisitor) (s : string) (pos : byref<int>) : bool =
    tryConsume_Char               v '{' s &pos
    && consume_WhiteSpace               s &pos
    && v.ObjectBegin ()
    && tryParse_ObjectMembers    true v s &pos
    && tryConsume_Char            v '}' s &pos
    && v.ObjectEnd ()

  and tryParse_Value (v : ParseVisitor) (s : string) (pos : byref<int>) : bool =
    if eos s pos then raiseEos v pos
    else
      let result = 
        match ch s pos with
        | 'n'                 -> tryParse_Null    v s &pos
        | 't'                 -> tryParse_True    v s &pos
        | 'f'                 -> tryParse_False   v s &pos
        | '['                 -> tryParse_Array   v s &pos
        | '{'                 -> tryParse_Object  v s &pos
        | '"'                 -> tryParse_String  v s &pos
        | c when isDigit c    -> tryParse_Number  v s &pos
        | _                   -> raiseValue v pos
      if result then ignore <| consume_WhiteSpace s &pos
      result

  let tryParse_Eos (v : ParseVisitor) (s : string) (pos : byref<int>) : bool =
    if eos s pos then true
    else
      v.Expected (pos, "EOS")
      false

  let tryParse (v : ParseVisitor) (s : string) (pos : byref<int>) : bool =
    consume_WhiteSpace s &pos
    && (tryParse_Object v s &pos) || (tryParse_Array v s &pos) 
    && consume_WhiteSpace s &pos
    && tryParse_Eos v s &pos


  type JsonBuilder =
    | BuilderRoot    of Json ref
    | BuilderObject  of ResizeArray<string*Json>
    | BuilderArray   of ResizeArray<Json>

  type JsonParseVisitor() =
    let expectedChars = ResizeArray<int*char> ()
    let expected      = ResizeArray<int*string> ()
    let unexpected    = ResizeArray<int*string> ()

    let context       = Stack<JsonBuilder> ()

    let mutable key   = ""
    let mutable root  = JsonNull

    let push v        = 
      context.Push v
      true

    let pop ()        = 
      match context.Pop () with
      | BuilderRoot vr    -> !vr
      | BuilderArray  vs  -> JsonArray (vs |> Seq.toList)
      | BuilderObject ms  -> JsonObject (ms |> Seq.toList)

    let add v         =
      match context.Peek () with
      | BuilderRoot vr    -> vr := v
      | BuilderObject ms  -> ms.Add (key, v)
      | BuilderArray  vs  -> vs.Add v
      true

    do
      context.Push <| (BuilderRoot <| ref JsonNull)

    interface ParseVisitor with
      override x.NullValue    ()      = add <| JsonNull
      override x.BoolValue    v       = add <| JsonBoolean v
      override x.NumberValue  v       = add <| JsonNumber v
      override x.StringValue  v       = add <| JsonString v

      override x.ArrayBegin   ()      = push (BuilderArray <| ResizeArray<_>())
      override x.ArrayEnd     ()      = add <| pop ()
      override x.ObjectBegin  ()      = push (BuilderObject <| ResizeArray<_>())
      override x.ObjectEnd    ()      = add <| pop ()
      override x.MemberKey    v       = key <- v; true

      override x.ExpectedChar (p,e)   = expectedChars.Add (p,e)
      override x.Expected     (p,e)   = expected.Add (p,e)
      override x.Unexpected   (p,ue)  = unexpected.Add (p,ue)

    member x.Root ()  = 
      Debug.Assert (context.Count = 1)
      pop ()

let parse (s : string) : ParseResult =
  let mutable pos = 0
    
  let v = Details.JsonParseVisitor ()

  match Details.tryParse (upcast v) s &pos with
  | true  ->
    Success (v.Root ())
  | false ->
    let reason = 
      sprintf 
        "Failed to parse JSON\n%s\n%s^\nExpected: %s\nUnexpected: %s"
        s
        (String ('-', pos))
        "N/A"
        "N/A"

    Failure (reason, pos)
