namespace BadCore.Nokia.Gateway

open BadCore.Extensions
open BadCore.Control
open BadCore.Text.Json
open BadCore.IO.Net
open BadCore.Nokia

module Radio =
    [<Struct>]
    type SignalStatus =
        { PhysicalCellID: string
          SNR: int
          RSRP: int
          RSRQ: int
          RSRPStrengthIndex: int
          SignalStrengthLevel: int
          Band: string }

    [<Struct>]
    type Status =
        { CellLTEStatus: SignalStatus
          Cell5GStatus: SignalStatus }

    let getStatus (): AsyncResult<Status, Gateway.Error> =
        let (>>=) = AsyncResult.op_GreaterGreaterEquals

        let parseRadioStatus json =
            { PhysicalCellID =
                  json
                  |> JsonElement.getProperty "PhysicalCellID"
                  |> JsonElement.getString
              SNR =
                  json
                  |> JsonElement.getProperty "SNRCurrent"
                  |> JsonElement.getInt
              RSRP =
                  json
                  |> JsonElement.getProperty "RSRPCurrent"
                  |> JsonElement.getInt
              RSRQ =
                  json
                  |> JsonElement.getProperty "RSRQCurrent"
                  |> JsonElement.getInt
              RSRPStrengthIndex =
                  json
                  |> JsonElement.getProperty "RSRPStrengthIndexCurrent"
                  |> JsonElement.getInt
              SignalStrengthLevel =
                  json
                  |> JsonElement.getProperty "SignalStrengthLevel"
                  |> JsonElement.getInt
              Band =
                  json
                  |> JsonElement.getProperty "Band"
                  |> JsonElement.getString }

        let parseResponse json =
            let cellLteStatArray =
                json
                |> JsonElement.getProperty "cell_LTE_stats_cfg"
                |> JsonElement.getArray

            let cell5gStatArray =
                json
                |> JsonElement.getProperty "cell_5G_stats_cfg"
                |> JsonElement.getArray

            if cellLteStatArray |> Seq.length > 1
            then failwithf $"Got more than 1 LTE Cell Status: %A{cellLteStatArray}"
            elif cell5gStatArray |> Seq.length > 1
            then failwithf $"Got more than 1 5G Cell Status: %A{cell5gStatArray}"

            let cellLteStat =
                Seq.head cellLteStatArray
                |> JsonElement.getProperty "stat"

            let cell5gStat =
                Seq.head cell5gStatArray
                |> JsonElement.getProperty "stat"

            Ok
                { CellLTEStatus = parseRadioStatus cellLteStat
                  Cell5GStatus = parseRadioStatus cell5gStat }

        let getResponseBody =
            HttpContent.readContentAsJson
            >> AsyncResult.mapError ResponseError
            >> Async.map (Result.bind parseResponse)

        let get =
            Http.get >> AsyncResult.mapError HttpError

        get (createUri "/fastmile_radio_status_web_app.cgi")
        >>= getResponseBody
