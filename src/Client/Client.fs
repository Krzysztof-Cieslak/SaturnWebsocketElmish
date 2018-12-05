module Client

open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch
open Fable
open Fable.Recharts
open Fable.Recharts.Props
open Thoth.Json
open Shared
open Fulma
open Fable.Import.JS

type Model =
    {Data : DataPoint []}

type Msg =
    | GenerateRandomData
    | GenerateInitalized
    | GenerateFailed
    | GetPoint of DataPoint

let init() : Model * Cmd<Msg> =
    let initialModel =
        {Data =
             [|{x = 1.
                y = 1.}
               {x = 2.
                y = 2.}
               {x = 3.
                y = 3.}|]}
    initialModel, Cmd.none

let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match msg with
    | GetPoint(pnt) -> 
        {currentModel with Data =
                               [|yield! currentModel.Data
                                 yield pnt|]}, Cmd.none
    | GenerateRandomData -> 
        let cmd =
            Cmd.ofPromise (fun _ -> fetch "/api/generate" []) () (fun _ -> GenerateInitalized) (fun _ -> GenerateFailed)
        currentModel, cmd
    | GenerateInitalized -> currentModel, Cmd.none
    | GenerateFailed -> currentModel, Cmd.none

let safeComponents =
    let components =
        span [] [a [Href "https://saturnframework.github.io"] [str "Saturn"]
                 str ", "
                 a [Href "http://fable.io"] [str "Fable"]
                 str ", "
                 a [Href "https://elmish.github.io/elmish/"] [str "Elmish"]
                 str ", "
                 a [Href "https://mangelmaxime.github.io/Fulma"] [str "Fulma"]]
    p [] [strong [] [str "SAFE Template"]
          str " powered by: "
          components]

let button txt onClick =
    Button.button [Button.IsFullWidth
                   Button.Color IsPrimary
                   Button.OnClick onClick] [str txt]

let chart data =
    Recharts.lineChart [Chart.Data data
                        Chart.Width 1000.
                        Chart.Height 300.] [line [Cartesian.Type Monotone
                                                  Cartesian.DataKey "y"] []
                                            xaxis [Cartesian.DataKey "x"] []
                                            yaxis [] []]

let view (model : Model) (dispatch : Msg -> unit) =
    div [] 
        [Navbar.navbar [Navbar.Color IsPrimary] [Navbar.Item.div [] [Heading.h2 [] [str "SAFE Template"]]]
         
         Container.container [] 
             [Content.content [] [chart model.Data
                                  button "Generate Random data" (fun _ -> dispatch GenerateRandomData)]]
         
         Footer.footer [] 
             [Content.content [Content.Modifiers [Modifier.TextAlignment(Screen.All, TextAlignment.Centered)]] 
                  [safeComponents]]]

let timer initial =
    let sub dispatch =
        let socket = Fable.Import.Browser.WebSocket.Create("ws://localhost:8085/ws")
        socket.addEventListener_open (fun _ -> console.log "Socket connected")
        socket.addEventListener_message (fun msg -> 
            let data = msg.data |> unbox<string>
            let pointRes = Thoth.Json.Decode.Auto.fromString<DataPoint> data
            match pointRes with
            | Error msg -> console.log ("Socket msg failed:", msg)
            | Ok msg -> dispatch (GetPoint msg))
        socket.addEventListener_close (fun _ -> console.log "Socket closed")
    Cmd.ofSub sub
#if DEBUG

open Elmish.Debug
open Elmish.HMR
#endif


Program.mkProgram init update view
|> Program.withSubscription timer
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif

|> Program.withReact "elmish-app"
// #if DEBUG
// |> Program.withDebugger
// #endif
|> Program.run
