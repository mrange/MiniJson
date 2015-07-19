# MiniJson

MiniJson is a conforming JSON parser for F# licensed under [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0)

MiniJson has decent performance (comparable to commonly used [Json.NET](http://www.newtonsoft.com/json))
and provides decent error messages (possible to suppress if performance is of importance).

The best way of referencing MiniJson is to use [Paket](http://www.nuget.org/packages/Paket/)
[http references](http://fsprojects.github.io/Paket/http-dependencies.html)

[paket.dependencies](http://fsprojects.github.io/Paket/dependencies-file.html)
```
http https://raw.githubusercontent.com/mrange/MiniJson/master/src/MiniJson/MiniJson.fs
```

[NuGet](http://www.nuget.org/packages/MiniJson/) can also be used to reference MiniJson

To install MiniJson using NuGet, run the following command in the [Package Manager Console](http://docs.nuget.org/consume/package-manager-console)
```
PM> Install-Package MiniJson
```

Using MiniJson is straight-forward
```fsharp
open Internal.MiniJson.JsonModule
open Internal.MiniJson.DynamicJsonModule

[<EntryPoint>]
let main argv =
  let jsonText = """[{"id":"123", "name":"Mr. Big", "age":30}, {"id":"123", "name":"Mr. X"}]"""

  match parse true jsonText with  // true for full error-info
  | Failure (msg, pos)  -> printfn "Failure@%d\n%s" pos msg
  | Success json        ->
    printfn "Success\n%s" <| toString true json  // true to indent JSON

    let root = json.Query

    for i = 0 to root.Length - 1 do
      let v     = root.[i]
      let id    = v?id.AsString
      let name  = v?name.AsString
      let age   = v?age.AsFloat
      printfn "Record - %d: id:%s, name:%s, age:%f" i id name age
  0
```

# TODO

1. Reduce MiniJson platform requirements
1. Improve MiniJson doc
1. Improve README.md
