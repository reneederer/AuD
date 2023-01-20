module Heap

open Elmish
open Fable.Remoting.Client
open Shared
open Browser
open Fable.Core.JsInterop
open System
open Fable.React.DrawingCanvas

type Heap =
    { Nodes : string
      Step : int
      SortOrder : SortOrder
    }

type HeapModel = Heap

type Msg =
    | HeapChanged of string
    | Step of int
    | SetSortOrder of SortOrder

let init () : HeapModel * Cmd<Msg> =
    let model =
        { //Nodes = "0,1,2,3,4,5,,,8,,10,,12,13,14"
          Nodes = "9,5,1,2,3,7,8,6,4,0"
          Step = 0
          SortOrder = SortOrder.Desc
        }

    model, Cmd.none

let update (msg: Msg) (model: HeapModel) : HeapModel * Cmd<Msg> =
    match msg with
    | HeapChanged nodesStr ->
        { model with Nodes = nodesStr }, Cmd.none
    | Step step ->
        { model with Step = step }, Cmd.none
    | SetSortOrder sortOrder ->
        { model with SortOrder = sortOrder; Step = 0 }, Cmd.none


open Feliz
open Feliz.Bulma
open Feliz.Bulma.Bulma
open type Feliz.Html
open Fable.React
open System


let heapify (heapStr : string) (step : int) (sortOrder : SortOrder) =
    if step = 0 then
        heapStr, Math.Max(step, 0)
    else
        let mutable currentStep = 0
        try
            let nodes = heapStr.Split(',') |> Array.map Int32.Parse

            let swap (i1 : int) (i2 : int) =
                let tmp = nodes.[i1]
                nodes.[i1] <- nodes.[i2]
                nodes.[i2] <- tmp

            let lastNonLeafIndex = (nodes.Length-1 - 1) / 2
            for i in lastNonLeafIndex .. -1 .. 0 do
                if currentStep < step then
                    if sortOrder = SortOrder.Desc && nodes.[i] < nodes.[i*2+1] ||
                           sortOrder = SortOrder.Asc && nodes.[i] > nodes.[i*2+1] then
                        currentStep <- currentStep + 1
                        swap i (i*2+1)
                    if currentStep < step && (sortOrder = SortOrder.Desc && nodes.[i] < nodes.[i*2+2] ||
                           sortOrder = SortOrder.Asc && nodes.[i] > nodes.[i*2+2]) then
                        swap i (i*2+2)
                        currentStep <- currentStep + 1
            (nodes |> Array.map string |> String.concat ",", Math.Max(currentStep, 0))
        with
        | _ ->
            "", Math.Max(step, 0)
        



let view (model: HeapModel) (dispatch: Msg -> unit) =
    let rec getRow n xs =
        if xs = [] then []
        else
            let a, b =
                try
                    xs |> List.splitAt (pown 2 n)
                with
                | _ -> xs, []
            console.log b.Length
            [a]@getRow (n + 1) b

    let nodeRows =
        model.Nodes.Split(',')
        |> List.ofSeq
        |> getRow 0
    let shiftY = 50.

    div
        []
        [ div
            []
            [ h2
                []
                [ str "Parent: (x-1)/2" ]
            ]
          drawingcanvas {
            Redraw = DrawFunction (fun ctx ->
                ctx.canvas.width <- 2000.0
                ctx.canvas.height <- 300.0
                ctx.font <- "15px Arial, plain"
                let mutable currentX = window.screen.width / 2.
                let mutable currentY = 50.
                let mutable shiftX = 800.
                for rowIndex in 0..nodeRows.Length-1 do
                    for columnIndex in 0..nodeRows[rowIndex].Length-1 do
                        ctx.fillText(
                            nodeRows.[rowIndex].[columnIndex],
                            currentX,
                            currentY)
                        if rowIndex > 0 && nodeRows.[rowIndex].[columnIndex] <> "" then
                            ctx.beginPath()
                            ctx.moveTo(currentX+5., currentY-16.)
                            ctx.lineTo(currentX+5.+((-1.)**columnIndex)*shiftX/2., currentY-shiftY+4.)
                            ctx.stroke()
                        currentX <- currentX + shiftX
                    currentY <- currentY + shiftY
                    shiftX <- shiftX / 2.
                    currentX <- (window.screen.width / 2.) - shiftX * (pown 2. rowIndex) + shiftX/2.
                ctx.stroke()
            )
            Props = [ ]
          }

          let nodeRows, steps =
              let numbers, steps =
                  heapify model.Nodes model.Step model.SortOrder
              numbers.Split(',')
              |> List.ofSeq
              |> getRow 0, steps


          Bulma.label
            [ prop.for' "desc"
              prop.children
                [ str "Desc"
                ]
            ]
          Bulma.input.radio
            [ prop.name "sortOrder"
              prop.id "desc"
              prop.isChecked (model.SortOrder = SortOrder.Desc)
              prop.onChange (fun (_ : Browser.Types.Event) -> dispatch <| SetSortOrder (SortOrder.Desc))
            ]
          Bulma.label
            [ prop.for' "asc"
              prop.children
                [ str "Asc"
                ]
            ]
          Bulma.input.radio
            [ prop.name "sortOrder"
              prop.id "asc"
              prop.isChecked (model.SortOrder = SortOrder.Asc)
              prop.onChange (fun (_ : Browser.Types.Event) -> dispatch <| SetSortOrder (SortOrder.Asc))
            ]
          h2 [] [str ($"CurrentStep: {steps}")]
          Bulma.button.button
            [ prop.onClick (fun _ -> dispatch (Step (steps + 1)))
              prop.children
                [ str $"Next"]
            ]
          Bulma.button.button
            [ prop.onClick (fun _ -> dispatch (Step (steps - 1)))
              prop.children
                [ str $"Previous"]
            ]


          drawingcanvas {
            Redraw = DrawFunction (fun ctx ->
                ctx.canvas.width <- 2000.0
                ctx.canvas.height <- 2000.0
                ctx.font <- "15px Arial, plain"
                let mutable currentX = window.screen.width / 2.
                let mutable currentY = 50.
                let mutable shiftX = 800.

                    
                for rowIndex in 0..nodeRows.Length-1 do
                    for columnIndex in 0..nodeRows[rowIndex].Length-1 do
                        ctx.fillText(
                            nodeRows.[rowIndex].[columnIndex],
                            currentX,
                            currentY)
                        if rowIndex > 0 && nodeRows.[rowIndex].[columnIndex] <> "" then
                            ctx.beginPath()
                            ctx.moveTo(currentX+5., currentY-16.)
                            ctx.lineTo(currentX+5.+((-1.)**columnIndex)*shiftX/2., currentY-shiftY+4.)
                            ctx.stroke()
                        currentX <- currentX + shiftX
                    currentY <- currentY + shiftY
                    shiftX <- shiftX / 2.
                    currentX <- (window.screen.width / 2.) - shiftX * (pown 2. rowIndex) + shiftX/2.
                ctx.stroke()
            )
            Props = [ Props.Style [] ]
          }

        ]








