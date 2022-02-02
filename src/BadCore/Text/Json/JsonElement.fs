namespace BadCore.Text.Json

module JsonElement =
    open System.Text.Json

    let deserialize (jsonString: string): Result<JsonElement, JsonElementError> =
        try
            Ok(JsonSerializer.Deserialize<JsonElement> jsonString)
        with
        | :? JsonException as ex -> Error(JsonElementError.from jsonString ex)

    let getArray (json: JsonElement): JsonElement seq = seq { yield! json.EnumerateArray() }

    let getProperty (key: string) (json: JsonElement): JsonElement = json.GetProperty(key)

    let getString (json: JsonElement): string = json.GetString()

    let getInt (json: JsonElement): int = json.GetInt32()

    let getUInt64 (json: JsonElement): uint64 = json.GetUInt64()
