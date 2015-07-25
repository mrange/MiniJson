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

namespace MiniJson.Adaptor
{
  using System;
  using System.Collections.Generic;
  using System.Diagnostics;
  using System.Dynamic;
  using System.Linq;
  using System.Runtime.Serialization;
  using System.Security.Permissions;

  using MiniJson;

  [Serializable]
  public class JsonParseException : Exception
  {
    const string Key_ErrorMessage   = "Key_ErrorMessage"  ;
    const string Key_ErrorPosition  = "Key_ErrorPosition" ;

    string  m_message   = "";
    int     m_position  = 0;

    public string ErrorMessage
    {
      get { return m_message; }
      internal set { m_message = value ?? "";}
    }

    public int ErrorPosition
    {
      get { return m_position; }
      internal set { m_position = value;}
    }

    public JsonParseException ()
    {
    }

    public JsonParseException (string message)
        : base (message)
    {
    }

    public JsonParseException (string message, Exception inner)
        : base (message, inner)
    {
    }

    protected JsonParseException (SerializationInfo info, StreamingContext context)
        : base (info, context)
    {
      if (info == null)
      {
        throw new ArgumentNullException ("info");
      }

      m_message   = info.GetString  (Key_ErrorMessage)  ;
      m_position  = info.GetInt32   (Key_ErrorPosition) ;
    }

    [SecurityPermission (SecurityAction.Demand, SerializationFormatter = true)]
    public override void GetObjectData (SerializationInfo info, StreamingContext context)
    {
      if (info == null)
      {
        throw new ArgumentNullException ("info");
      }

      info.AddValue (Key_ErrorMessage  , m_message );
      info.AddValue (Key_ErrorPosition , m_position);

      base.GetObjectData (info, context);
    }
  }

  public sealed class JsonDynamicObject : DynamicObject
  {
    readonly DynamicJsonModule.JsonPath m_jsonPath;

    public JsonDynamicObject (DynamicJsonModule.JsonPath jsonPath)
    {
      Debug.Assert (jsonPath != null);
      m_jsonPath = jsonPath;
    }

    public override string ToString ()
    {
      return m_jsonPath.AsString;
    }

    public DynamicJsonModule.JsonPath GetJsonPath ()
    {
      return m_jsonPath;
    }

    public string GetJsonPathString ()
    {
      return m_jsonPath.ToString ();
    }

    public JsonModule.Json GetJson ()
    {
      if (m_jsonPath.IsPathError)
      {
        return JsonModule.Json.JsonNull;
      }

      var pathOk = (DynamicJsonModule.JsonPath.PathOk) m_jsonPath;

      return pathOk.Item.Item1;
    }

    public string GetJsonString ()
    {
      return GetJsonString (false);
    }

    public string GetJsonString (bool indent)
    {
      return GetJson ().ToString (indent);
    }

    public int GetLength ()
    {
      return m_jsonPath.Length;
    }

    public bool HasValue ()
    {
      return m_jsonPath.HasValue;
    }

    public override IEnumerable<string> GetDynamicMemberNames ()
    {
      return m_jsonPath.Keys;
    }

    public override bool TryGetIndex (GetIndexBinder binder, object[] indexes, out object result)
    {
      if (indexes.Length == 1 && indexes[0] is int)
      {
        var index = (int)indexes[0];
        var next  = m_jsonPath.Item (index);

        result = new JsonDynamicObject (next);

        return true;
      }

      return base.TryGetIndex (binder, indexes, out result);
    }

    public override bool TryGetMember (GetMemberBinder binder, out object result)
    {
      var next  = m_jsonPath.Get (binder.Name);

      result = new JsonDynamicObject (next);

      return true;
    }

    /*
    public override bool TryInvokeMember (InvokeMemberBinder binder, object[] args, out object result)
    {
        var entity = m_entities.FirstOrEmpty ();

        var dynamicObject = entity as DynamicObject;
        if (dynamicObject != null)
        {
            return dynamicObject.TryInvokeMember (binder, args, out result);
        }

        return base.TryInvokeMember (binder, args, out result);
    }
    */

    public override bool TryConvert (ConvertBinder binder, out object result)
    {
        var returnType = binder.ReturnType;

        if (returnType == typeof (object[]))
        {
          result = m_jsonPath.Children.Select (c => new JsonDynamicObject (c)).ToArray ();
          return true;
        }
        else if (returnType == typeof (string))
        {
          result = m_jsonPath.AsString;
          return true;
        }
        else if (returnType == typeof (double))
        {
          result = m_jsonPath.AsFloat;
          return true;
        }
        else if (returnType == typeof (bool))
        {
          result = m_jsonPath.AsBool;
          return true;
        }

        return base.TryConvert (binder, out result);
    }
  }

  public static class JsonSerializer
  {
    public static bool TryParse (string input, out JsonModule.Json result, out string errorMessage, out int errorPosition)
    {
      result        = null;
      errorMessage  = null;
      errorPosition = 0   ;

      var json = JsonModule.parse (true, input ?? "");

      if (json.IsSuccess)
      {
        var jsonSucces  = (JsonModule.ParseResult.Success) json;
        result          = jsonSucces.Item ?? JsonModule.Json.JsonNull;
        return true;
      }
      else
      {
        var jsonFailure = (JsonModule.ParseResult.Failure) json;
        errorMessage    = jsonFailure.Item1 ?? "";
        errorPosition   = jsonFailure.Item2;
        return false;
      }
    }

    public static bool TryParse (string input, out JsonModule.Json result, out int errorPosition)
    {
      result        = null;
      errorPosition = 0   ;

      var json = JsonModule.parse (false, input ?? "");

      if (json.IsSuccess)
      {
        var jsonSucces  = (JsonModule.ParseResult.Success) json;
        result          = jsonSucces.Item ?? JsonModule.Json.JsonNull;
        return true;
      }
      else
      {
        var jsonFailure = (JsonModule.ParseResult.Failure) json;
        errorPosition   = jsonFailure.Item2;
        return false;
      }
    }


    public static JsonModule.Json Parse (string input)
    {
      JsonModule.Json result        ;
      string          errorMessage  ;
      int             errorPosition ;
      if (TryParse (input, out result, out errorMessage, out errorPosition))
      {
        return result;
      }
      else
      {
        var e = new JsonParseException () ;
        e.ErrorMessage  = errorMessage    ;
        e.ErrorPosition = errorPosition   ;
        throw e                           ;
      }
    }

    static JsonDynamicObject CreateDynamicObject (JsonModule.Json json)
    {
      Debug.Assert (json != null);

      var jsonPath  = DynamicJsonModule.makePath (json);
      return new JsonDynamicObject (jsonPath);
    }

    public static bool TryParseAsDynamic (string input, out dynamic result, out string errorMessage, out int errorPosition)
    {
      result = null;

      JsonModule.Json json;

      if (!TryParse (input, out json, out errorMessage, out errorPosition))
      {
        return false;
      }

      result = CreateDynamicObject (json);

      return true;
    }

    public static bool TryParseAsDynamic (string input, out dynamic result, out int errorPosition)
    {
      result = null;

      JsonModule.Json json;

      if (!TryParse (input, out json, out errorPosition))
      {
        return false;
      }

      result = CreateDynamicObject (json);

      return true;
    }

    public static dynamic ParseAsDynamic (string input)
    {
      var json = Parse (input);

      return CreateDynamicObject (json);
    }
  }
}
