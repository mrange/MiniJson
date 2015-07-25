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

namespace MiniJson.Adaptor.Tests
{
  using System;

  using MiniJson.Adaptor;

  class Program
  {
    static void Main(string[] args)
    {
      dynamic json  = JsonSerializer.ParseAsDynamic (@"{""id"":""Hello"",""vs"":[1,2,3, null, true,false,[1,2,3]]}");
      Console.WriteLine (json);
      string id     = json.id;
      Console.WriteLine (id);
      Console.WriteLine (json.id.GetJsonString ());
      dynamic[] vs  = json.vs;
      foreach (dynamic v in vs)
      {
        bool   b = v;
        double d = v;
        string s = v;
        string j = v.GetJsonString ();
        Console.WriteLine ("Bool  : {0}"  , b);
        Console.WriteLine ("Double: {0}"  , d);
        Console.WriteLine ("String: {0}"  , s);
        Console.WriteLine ("Json  : {0}"  , j);

        dynamic[] cs = v;
        foreach (dynamic c in cs)
        {
          Console.WriteLine (c);
        }
      }
    }
  }
}
