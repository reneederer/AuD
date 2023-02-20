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

      FloydVertices : string
      FloydInitialWeights : string list
    }

type Msg =
    | HornerSchemaChanged of (string * string * string)
    | ModChanged of (string * string)
    | FloydVerticesChanged of string
    | FloydInitialWeightsChanged of (int * string)

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

              FloydVertices = "a,b,c,d"
              FloydInitialWeights = ["1000"; "1"; "4"; "3"; "1000"; "8"; "1000"; "1000"; "2"; "1000"; "5"; "1000"]
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
    | model, FloydVerticesChanged s ->
        { model
            with
                FloydVertices = s
                FloydInitialWeights =
                    []
        }, Cmd.none
    | model, FloydInitialWeightsChanged (index, weight) ->
        console.log $"xx:{index}, {weight}"
        let xs =
            List.init
                (Math.Max(index + 1, model.FloydInitialWeights.Length))
                (fun i ->
                    if i = index then
                        weight
                    elif i < model.FloydInitialWeights.Length then
                        model.FloydInitialWeights.[i]
                    else
                        "999999")
        console.log $"xs:{xs}"
        { model
            with
                FloydInitialWeights = xs
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

        let getVerticesCombs (vertices : string list) =
            [ for i in 0..vertices.Length - 1 do
                for j in 0..vertices.Length - 1 do
                    if i <> j then
                        vertices.[i], vertices.[j]
            ]

        let vertices =
            model.FloydVertices.Split "," |> List.ofSeq |> List.distinct

        let verticesCombs = getVerticesCombs vertices

        let actualEdges =
            try
                [ for i in 0 .. model.FloydInitialWeights.Length - 1 do
                    (verticesCombs.[i], Double.Parse model.FloydInitialWeights.[i])
                ]
                |> Map.ofList
            with
            | _ ->
              console.log "err1";   Map.empty

        printfn $"aaa: {actualEdges}"


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
        printfn $"floydedges: {floydEdges}"

        let getMin (edge : string * string) =
            try
                floydEdges.[edge]
                |> Map.fold (fun state k v ->
                    if v < state then v else state
                ) 999999.
            with
            | _ ->
                console.log $"err2: {edge}";
                999999.
            

        for k in 0..vertices.Length-1 do
            for i in 0..vertices.Length-1 do
                for j in 0..vertices.Length-1 do
                    if vertices.[i] <> vertices.[j] then
                        let min = getMin (vertices.[i], vertices.[j])
                        let current = getMin (vertices.[i], vertices.[k]) + getMin(vertices.[k], vertices.[j])
                        floydEdges <-
                            floydEdges.Add((vertices.[i], vertices.[j]),
                            floydEdges.[vertices.[i], vertices.[j]].Add(vertices.[k], if current < min then current else min))
        printfn $"{floydEdges}"
        printfn $"{vertices}"

        Bulma.label
            [ str "Floyd"]
        Bulma.input.text
            [ prop.onChange (fun (evt : Browser.Types.Event) ->
                dispatch <| FloydVerticesChanged evt.target?value)
              prop.value (string model.FloydVertices)
            ]
        let mutable n = 0
        Bulma.table
            [ for i in 0..vertices.Length - 1 do
                  for j in 0..vertices.Length - 1 do
                    if vertices.[i] <> vertices.[j] then
                        tr
                            []
                            [ td
                                []
                                [ str (try $"{vertices.[i]}, {vertices.[j]}" with | _ -> console.log "err3"; "") ]
                              let m = n
                              td
                                []
                                [ Bulma.input.text
                                    [ prop.onChange (fun (evt : Browser.Types.Event) ->
                                        dispatch <| FloydInitialWeightsChanged (m, evt.target?value))
                                      prop.value (try (string model.FloydInitialWeights.[m]) with | _ -> console.log "err4"; "")
                                    ]
                                ]
                              console.log $"n: {n}"
                              n <- n + 1
                              for k in 0..vertices.Length - 1 do
                                td
                                    []
                                    [ str (try (string floydEdges.[vertices.[i], vertices.[j]].[vertices.[k]]) with | _ ->  console.log "err5";"") ]

                            ]
            ]
        ]










