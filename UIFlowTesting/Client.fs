namespace UIFlowTesting

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Templating
open WebSharper.UI.Html
open WebSharper.Forms
open WebSharper.Sitelets

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

    let RenderForm1 (actions: FlowActions<_>) v resv =
        div [] [
            text "First input: "
            Doc.InputType.Text [] v
            Doc.Button "Next" [] (fun () -> actions.Next resv)
            Doc.Button "Cancel" [] actions.Cancel
        ]

    let RenderForm2 (actions: FlowActions<_>) v resv =
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
        Flow.End (
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

    let Cancelled (actions: CancelledFlowActions) =
        div [] [
            text "Canceled"
            Doc.Button "Restart" [] actions.Restart
        ]    

    type EndPoint =
        | Home
        | Form

    [<SPAEntryPoint>]
    let Main () =
        let router = Router.Infer<EndPoint>()
        
        let currentRoute =
            router 
            |> Router.InstallHash Home

        currentRoute.View.Doc (fun route ->
            match route with
            | Home ->
                div [] [
                    text "Hello"
                    p [] [ a [ attr.href (router.HashLink Form) ] [ text "Go to form"] ]
                ]
            | Form ->
                div [] [ 
                    CombinedFlow()
                    |> Flow.EmbedWithCancel Cancelled
                    p [] [ a [ attr.href (router.HashLink Home) ] [ text "Go to home"] ]
                ]
        )
        |> Doc.RunById "main"

    //type EndPoint =
    //    | Home
    //    | Form of int

    //[<SPAEntryPoint>]
    //let Main () =
    //    let router = Router.Infer<EndPoint>()
        
    //    let currentRoute =
    //        router 
    //        |> Router.InstallHash Home

    //    let formPageCached =
    //        lazy
    //            let flowVar = 
    //                currentRoute.Lens (function Home -> 0 | Form i -> i) (fun ep i -> match ep with Form _ -> Form i | _ -> ep)
    //            div [] [ 
    //                CombinedFlow()
    //                |> Flow.EmbedWithRoutingAndCancel flowVar Cancelled
    //                p [] [ a [ attr.href (router.HashLink Home) ] [ text "Go to home"] ]
    //            ]

    //    currentRoute.View.Doc (fun route ->
    //        match route with
    //        | Home ->
    //            div [] [
    //                text "Hello"
    //                p [] [ a [ attr.href (router.HashLink (Form 1)) ] [ text "Go to form"] ]
    //            ]
    //        | Form _ ->
    //            formPageCached.Value
    //    )
    //    |> Doc.RunById "main"
