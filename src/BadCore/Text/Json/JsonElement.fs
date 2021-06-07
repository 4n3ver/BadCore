namespace BadCore.Text.Json

open System.Text.Json

module JsonElement =
    let deserialize (jsonString: string): JsonElement =
        JsonSerializer.Deserialize<JsonElement> jsonString

    let getArray (json: JsonElement): JsonElement seq = seq { yield! json.EnumerateArray() }

    let getProperty (key: string) (json: JsonElement): JsonElement = json.GetProperty(key)

    let getString (json: JsonElement): string = json.GetString()

    let getInt (json: JsonElement): int = json.GetInt32()

    let getUInt64 (json: JsonElement): uint64 = json.GetUInt64()
