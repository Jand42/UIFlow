namespace UIFlowTesting

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Templating
open WebSharper.UI.Html
open WebSharper.Forms

open UIFlowTesting // shadowing for Flow

[<JavaScript>]
module Client =

    let Form1() =
        Console.Log("Form 1 created")
        Form.Yield "A"
        |> Form.TransmitView

    let Form2() =
        Console.Log("Form 2 created")
        Form.Yield "B"
        |> Form.TransmitView

    let RenderForm1 actions v (resv: View<_>) =
        div [] [
            text "First input: "
            Doc.InputType.Text [] v
            Doc.Button "Next" [] (fun () -> actions.Next resv)
            Doc.Button "Cancel" [] actions.Cancel
        ]

    let RenderForm2 actions v (resv: View<_>) =
        div [] [
            text "Second input: "
            Doc.InputType.Text [] v
            Doc.Button "Back" [] actions.Back
            Doc.Button "Submit" [] (fun () -> actions.Next resv)
            Doc.Button "Cancel" [] actions.Cancel
        ]

    let Form1Flow() = 
        Flow.Define (fun actions ->
            Form1()
            |> Form.Render (RenderForm1 actions)
        )

    let Form2Flow() = 
        Flow.Define (fun actions ->
            Form2()
            |> Form.Render (RenderForm2 actions)
        )

    let ResultFlow (x: View<Result<string>>) (y: View<Result<string>>) =
        Flow.Static (
            div [] [
                text $"""First: {match x.V with Success o -> o | _ -> ""} Second: {match y.V with Success o -> o | _ -> ""}"""    
            ]
        )

    let CombinedFlow() =
        flow {
            let! x = Form1Flow()
            let! y = Form2Flow()
            return! ResultFlow x y
        }

    let Cancelled actions =
        div [] [
            text "Canceled"
            Doc.Button "Restart" [] actions.Restart
        ]    

    [<SPAEntryPoint>]
    let Main () =
        div [] [ 
            CombinedFlow()
            |> Flow.EmbedWithCancel Cancelled
        ]
        |> Doc.RunById "main"
