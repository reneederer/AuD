module Index

open Elmish
open Fable.Remoting.Client
open Shared
open Browser
open Fable.Core.JsInterop
open System

type Model =
    { HornerSchemaSourceBase : string
      HornerSchemaTargetBase : string
      HornerSchemaDigits : string

      ModMods : string
      ModNumbers : string

      Floyd : string
    }

type Msg =
    | HornerSchemaChanged of (string * string * string)
    | ModChanged of (string * string)
    | FloydChanged of string

let todosApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

let init () : Model * Cmd<Msg> =
    let model =
            { HornerSchemaSourceBase = "16"
              HornerSchemaTargetBase = "10"
              HornerSchemaDigits = "10,9,15,2"

              ModMods = "8"
              ModNumbers = "31,42,36"

              Floyd = "a,b,1000 a,c,1 a,d,4 b,a,3 b,c,1000 b,d,8 c,a,1000 c,b,1000 c,d,2 d,a,1000 d,b,5 d,c,1000"
            }
    model, Cmd.none
        ////(Heap.init ()) |> (fun (model, cmd) -> Heap model, cmd |> Cmd.map HeapMsg)
        //(Graph.init ()) |> (fun (model, cmd) -> Graph model, cmd |> Cmd.map GraphMsg)

let update (msg: Msg) (model: Model) : (Model * Cmd<Msg>) =
    match model, msg with
    | model, HornerSchemaChanged (s, d, ns) ->
        { model
            with
                HornerSchemaSourceBase = s
                HornerSchemaTargetBase = d
                HornerSchemaDigits = ns
         }, Cmd.none
    | model, ModChanged (mods, numbers) ->
        { model
            with
                ModMods = mods
                ModNumbers = numbers
         }, Cmd.none
    | model, FloydChanged s ->
        { model
            with
                Floyd = s
        }, Cmd.none

let toBase (s : string) (d : string) (ns : string) =
    let s = Int32.Parse s
    let d = Int32.Parse d
    let ns = ns.Split (',') |> List.ofSeq
    ns
    |> List.mapi (fun i n ->
        let n = Int32.Parse n
        n * int(Math.Pow(s, double(ns.Length - i)))
    )

    


let getHorner (hs: Model) =
    try
        let sourceBase = Int32.Parse hs.HornerSchemaSourceBase
        let digits = hs.HornerSchemaDigits.Split [|','|] |> List.ofSeq |> List.map Int32.Parse

        let mutable sum = 0
        [ for digit in digits do
            sum <- sum * sourceBase
            yield sum
            sum <- sum + digit
            yield sum
        ]
    with
    | _ ->
        []

let rec fac n =
    if n = 0 then 1
    else n * fac (n - 1)

open Feliz
open Feliz.Bulma
open Feliz.Bulma.Bulma
open type Feliz.Html
open Fable.React
open System

let view (model: Model) (dispatch: Msg -> unit) =
    div
      []
      [ h2
          []
          [ str "HornerSchema" ]
        Bulma.label
            [ str "Source base"]
        Bulma.input.text
            [ prop.onChange (fun (evt : Browser.Types.Event) ->
                dispatch <| HornerSchemaChanged (evt.target?value, model.HornerSchemaTargetBase, model.HornerSchemaDigits))
              prop.value (string model.HornerSchemaSourceBase)

            ]
        br []
        Bulma.label
            [ str "Target base"]
        Bulma.input.text
            [ prop.onChange (fun (evt : Browser.Types.Event) ->
                dispatch <| HornerSchemaChanged (model.HornerSchemaSourceBase, evt.target?value, model.HornerSchemaDigits))
              prop.value (string model.HornerSchemaTargetBase)
            ]
        br []
        Bulma.label
            [ str "Number to convert"]
        Bulma.input.text
            [ prop.onChange (fun (evt : Browser.Types.Event) ->
                dispatch <| HornerSchemaChanged (model.HornerSchemaSourceBase, model.HornerSchemaTargetBase, evt.target?value))
              prop.value model.HornerSchemaDigits
            ]

        let horner = getHorner model

        Bulma.table
            [ tr
                []
                [ for i in 0..2..horner.Length - 1 do
                    td
                      []
                      [ str (string horner[i]) ]
                ]
              tr
                []
                [ for i in 1..2..horner.Length - 1 do
                    td
                      []
                      [ str (string horner[i]) ]
                ]
            ]


        br []
        hr []
        br []
        h2
          []
          [ str "Mod" ]
        div
          []
          [ Bulma.label
                [ str "Mod"]
            Bulma.input.text
                [ prop.onChange (fun (evt : Browser.Types.Event) ->
                    dispatch <| ModChanged (evt.target?value, model.ModNumbers))
                  prop.value (string model.ModMods)
                ]
            Bulma.input.text
                [ prop.onChange (fun (evt : Browser.Types.Event) ->
                    dispatch <| ModChanged (model.ModMods, evt.target?value))
                  prop.value (string model.ModNumbers)
                ]

            let modMods =
                try
                    model.ModMods.Split(",", StringSplitOptions.RemoveEmptyEntries) |> Seq.map Int32.Parse
                with
                | _ ->
                    [1]
                
            let modNumbers =
                try
                    model.ModNumbers.Split(",", StringSplitOptions.RemoveEmptyEntries) |> Seq.map Int32.Parse
                with
                | _ ->
                    []
            Bulma.table
                [ for modNumber in modNumbers do
                    let mutable n = modNumber
                    for modMod in modMods do
                        n <- n % modMod
                    tr
                        []
                        [ td
                            []
                            [ str (string modNumber) ]
                          td
                            []
                            [ str (string n) ]
                        ]
                ]
          ]
        br []
        hr []
        br []
        h2 [] [ str "Floyd" ]

        let actualEdges =
            try
                [ for edgeStr in model.Floyd.Split " " do
                    let [| a; b; weightStr |] = edgeStr.Split ","
                    ((a, b), Double.Parse weightStr)
                ]
                |> Map.ofList
            with
            | _ ->
                Map.empty

        let vertices =
            actualEdges |> Map.toList |> List.collect (fun ((a, b), _) -> [a; b])
            |> List.distinct


        let mutable floydEdges : Map<string * string, Map<string, double>> =
            [ for i in 0..vertices.Length-1 do
                for j in 0..vertices.Length-1 do
                    if vertices.[i] <> vertices.[j] then
                        (vertices.[i], vertices.[j]),
                        [ for k in 0..vertices.Length-1 do
                                if not <| actualEdges.ContainsKey (vertices.[i], vertices.[j]) then
                                    (vertices.[k], 999999.)
                                else 
                                    vertices.[k], actualEdges.[(vertices.[i], vertices.[j])]
                        ]
                        |> Map.ofList
            ]
            |> Map.ofList

        let getMin (edge : string * string) =
            try
                floydEdges.[edge]
                |> Map.fold (fun state k v ->
                    if v < state then v else state
                ) 999999.
            with
            | _ -> 999999.
            

        let floydEdgesList = floydEdges |> Map.toList
        for k in 0..vertices.Length-1 do
            for i in 0..vertices.Length-1 do
                for j in 0..vertices.Length-1 do
                    if vertices.[i] <> vertices.[j] then
                        let min = getMin (vertices.[i], vertices.[j])
                        let current = getMin (vertices.[i], vertices.[k]) + getMin(vertices.[k], vertices.[j])
                        floydEdges <-
                            floydEdges.Add((vertices.[i], vertices.[j]),
                            floydEdges.[vertices.[i], vertices.[j]].Add(vertices.[k], if min > current then current else min))
        printfn $"{floydEdges}"
        printfn $"{vertices}"

        let n = 10

        Bulma.label
            [ str "Floyd"]
        Bulma.input.text
            [ prop.onChange (fun (evt : Browser.Types.Event) ->
                dispatch <| FloydChanged evt.target?value)
              prop.value (string model.Floyd)
            ]
        Bulma.table
            [ for i in 0..vertices.Length - 1 do
                  for j in 0..vertices.Length - 1 do
                    if vertices.[i] <> vertices.[j] then
                        tr
                            []
                            [ td
                                []
                                [ str (try $"{vertices.[i]}, {vertices.[j]}" with | _ -> "") ]
                              for k in 0..vertices.Length - 1 do
                                td
                                    []
                                    [ str (try (string floydEdges.[vertices.[i], vertices.[j]].[vertices.[k]]) with | _ -> "") ]

                            ]
            ]
        ]










