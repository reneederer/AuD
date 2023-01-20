module Index

open Elmish
open Fable.Remoting.Client
open Shared
open Browser
open Fable.Core.JsInterop
open System
open Heap

type HornerSchema =
    { SourceBase : string
      TargetBase : string
      Digits : string
    }

type HashTable =
    { BoxCount : int
      HashF : string -> int
      SecondHashF : string -> int
      JumpCount : int
    }

type AvlTree =
    { Nodes : (int option) list
    }

type Model =
    | HornerSchema of HornerSchema
    | HashTable of HashTable
    | AvlTree of AvlTree
    | Heap of Heap.HeapModel

type Msg =
    | HornerSchemaChanged of (string * string * string)
    | HeapMsg of Heap.Msg

let todosApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

let init () : Model * Cmd<Msg> =
    //let model =
        //HornerSchema
        //    { SourceBase = "16"
        //      TargetBase = "10"
        //      Digits = "10,9,15,2"
        //    }
        (Heap.init ()) |> (fun (x, y) -> Heap x, y |> Cmd.map HeapMsg)

let update (msg: Msg) (model: Model) : (Model * Cmd<Msg>) =
    match model, msg with
    | HornerSchema hornerSchema, HornerSchemaChanged (s, d, ns) ->
        (HornerSchema
            { SourceBase = s
              TargetBase = d
              Digits = ns
            }), Cmd.none

    | AvlTree avlTree, _ -> //AvlTreeChanged s ->
        model, Cmd.none

    | Heap heap, HeapMsg msg ->
        Heap.update msg heap
        |> fun (x, y) -> Heap x, Cmd.map HeapMsg y



let toBase (s : string) (d : string) (ns : string) =
    let s = Int32.Parse s
    let d = Int32.Parse d
    let ns = ns.Split (',') |> List.ofSeq
    ns
    |> List.mapi (fun i n ->
        let n = Int32.Parse n
        n * int(Math.Pow(s, float(ns.Length - i)))
    )

    


let getHorner (hs: HornerSchema) =
    try
        let sourceBase = Int32.Parse hs.SourceBase
        let digits = hs.Digits.Split [|','|] |> List.ofSeq |> List.map Int32.Parse

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


open Feliz
open Feliz.Bulma
open Feliz.Bulma.Bulma
open type Feliz.Html
open Fable.React
open System

let view (model: Model) (dispatch: Msg -> unit) =
    match model with
    | Heap heap ->
        Heap.view heap (fun msg -> dispatch (HeapMsg msg))
    | HornerSchema hornerSchema ->
        div
          []
          [ h2
              []
              [ str "HornerSchema"
                Bulma.label
                    [ str "Source base"]
                Bulma.input.text
                    [ prop.onChange (fun (evt : Browser.Types.Event) ->
                        dispatch <| HornerSchemaChanged (evt.target?value, hornerSchema.TargetBase, hornerSchema.Digits))
                      prop.value (string hornerSchema.SourceBase)

                    ]
                br []
                Bulma.label
                    [ str "Target base"]
                Bulma.input.text
                    [ prop.onChange (fun (evt : Browser.Types.Event) ->
                        dispatch <| HornerSchemaChanged (hornerSchema.SourceBase, evt.target?value, hornerSchema.Digits))
                      prop.value (string hornerSchema.TargetBase)
                    ]
                br []
                Bulma.label
                    [ str "Number to convert"]
                Bulma.input.text
                    [ prop.onChange (fun (evt : Browser.Types.Event) ->
                        dispatch <| HornerSchemaChanged (hornerSchema.SourceBase, hornerSchema.TargetBase, evt.target?value))
                      prop.value hornerSchema.Digits
                    ]

                let horner = getHorner hornerSchema

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
                        
                    

              ]
            h2
              []
              [ str "Closed HashTable" ]


            h2
              []
              [ str "Tree"]








      ]
