// ----------------------------------------------------------------------------------------------
// Copyright 2015 Mårten Rånge
//
// Licensed under the Apache License, Version 2.0 (the ""License"");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an ""AS IS"" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
// ----------------------------------------------------------------------------------------------

namespace MiniJson.Adaptor.Tests
{
  using System;

  using MiniJson.Adaptor;

  class Program
  {
    static void Main(string[] args)
    {
      var jsonText = @"[{""id"":""123"", ""name"":""Mr. Big"", ""age"":30}, {""id"":""123"", ""name"":""Mr. X""}]";

      var jsonParser = new JsonParser (jsonText, true);

      Console.WriteLine ("ParseResult: {0}", jsonParser);

      dynamic[] users = jsonParser.DynamicResult.GetChildren ();

      foreach (dynamic user in users)
      {
        string id     = user.id                       ;
        string name   = user.name                     ;
        double age    = user.age.ConvertToFloat (-1.0);
        Console.WriteLine ("Record: id:{0}, name:{1}, age:{2}", id, name, age);
      }
    }
  }
}
