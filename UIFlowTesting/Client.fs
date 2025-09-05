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

    //let CombinedFlow() =
    //    flow {
    //        let! x = Form1Flow()
    //        let! y = Form2Flow()
    //        return! ResultFlow x y
    //    }

    //let Cancelled (actions: CancelledFlowActions) =
    //    div [] [
    //        text "Canceled"
    //        Doc.Button "Restart" [] actions.Restart
    //    ]    

    //type EndPoint =
    //    | Home
    //    | Form

    //[<SPAEntryPoint>]
    //let Main () =
    //    let router = Router.Infer<EndPoint>()
        
    //    let currentRoute =
    //        router 
    //        |> Router.InstallHash Home

    //    currentRoute.View.Doc (fun route ->
    //        match route with
    //        | Home ->
    //            div [] [
    //                text "Hello"
    //                p [] [ a [ attr.href (router.HashLink Form) ] [ text "Go to form"] ]
    //            ]
    //        | Form ->
    //            div [] [ 
    //                p [] [ text "Form 1" ]
    //                CombinedFlow()
    //                |> Flow.EmbedWithCancel Cancelled
    //                p [] [ text "Form 2" ]
    //                CombinedFlow()
    //                |> Flow.EmbedWithCancel Cancelled
    //                p [] [ a [ attr.href (router.HashLink Home) ] [ text "Go to home"] ]
    //            ]
    //    )
    //    |> Doc.RunById "main"


    //let Stage1 (actions: FlowActions<_>) =
    //    let nameVar = Var.Create ""
    //    div [] [
    //        p [] [ 
    //            text "Name: " 
    //            Doc.InputType.Text [] nameVar
    //        ]
    //        p [] [
    //            Doc.Button "Next" [ attr.style "margin-right: 0.5em" ] (fun () -> 
    //                if nameVar.Value = "" then
    //                    JS.Alert "Please provide a name"
    //                else
    //                    actions.Next nameVar.Value
    //            )
    //            Doc.Button "Cancel" [] actions.Cancel
    //        ]
    //    ]

    //let Stage2 (actions: FlowActions<_>) =
    //    let ageVar = Var.Create (CheckedInput.Make 1)
    //    div [] [
    //        p [] [ 
    //            text "Age: " 
    //            Doc.InputType.Int [] ageVar
    //        ]
    //        p [] [
    //            Doc.Button "Back" [ attr.style "margin-right: 0.5em" ] actions.Back
    //            Doc.Button "Submit" [ attr.style "margin-right: 0.5em" ] (fun () -> 
    //                match ageVar.Value with
    //                | Valid (a, _) ->
    //                    if 0 <= a && a <= 99 then
    //                        actions.Next a
    //                    else
    //                        JS.Alert "Please provide and age between 0 and 99"
    //                | _ -> 
    //                    JS.Alert "Please enter a valid number"
    //            )
    //            Doc.Button "Cancel" [] actions.Cancel
    //        ]
    //    ]

    //let StageEnd (name: View<string>) (age: View<int>) =
    //    div [] [
    //        p [] [ 
    //            text $"Hello {name.V}, age {age.V}" 
    //        ]
    //    ]

    //let CombinedFlow() =
    //    flow {
    //        let! name = Flow.Define Stage1 |> Flow.View
    //        let! age = Flow.Define Stage2 |> Flow.View
    //        return! Flow.End (StageEnd name age)
    //    }

    let Stage1 (actions: FlowActions<_>) =
        let nameVar = Var.Create ""
        div [] [
            p [] [ 
                text "Name: " 
                Doc.InputType.Text [] nameVar
            ]
            p [] [
                Doc.Button "Next" [ attr.style "margin-right: 0.5em" ] (fun () -> 
                    if nameVar.Value = "" then
                        JS.Alert "Please provide a name"
                    else
                        actions.Next nameVar.View
                )
                Doc.Button "Cancel" [] actions.Cancel
            ]
        ]

    let Stage2 (actions: FlowActions<_>) =
        let ageVar = Var.Create (CheckedInput.Make 1)
        div [] [
            p [] [ 
                text "Age: " 
                Doc.InputType.Int [] ageVar
            ]
            p [] [
                Doc.Button "Back" [ attr.style "margin-right: 0.5em" ] actions.Back
                Doc.Button "Submit" [ attr.style "margin-right: 0.5em" ] (fun () -> 
                    actions.Next ageVar.View
                    //match ageVar.Value with
                    //| Valid (a, _) ->
                    //    if 0 <= a && a <= 99 then
                    //        actions.Next a
                    //    else
                    //        JS.Alert "Please provide and age between 0 and 99"
                    //| _ -> 
                    //    JS.Alert "Please enter a valid number"
                )
                Doc.Button "Cancel" [] actions.Cancel
            ]
        ]

    let StageEnd (name: View<string>) (age: View<int>) =
        div [] [
            p [] [ 
                text $"Hello {name.V}, age {age.V}" 
            ]
        ]

    let CombinedFlow() =
        flow {
            let! name = Flow.Define Stage1 |> Flow.ValidateView (fun x -> x <> "")
            let! age = 
                Flow.Define Stage2 
                |> Flow.ValidateView (
                    function
                    | Valid (a, _) ->
                        if 0 <= a && a <= 99 then
                            true
                        else
                            JS.Alert "Please provide and age between 0 and 99"
                            false
                    | _ -> 
                        JS.Alert "Please enter a valid number"
                        false
                )
                |> Flow.Map (View.Map (function Valid (a, _) -> a | _ -> 0))
            return! Flow.End (StageEnd name age)
        }

    let Cancelled (actions: EndedFlowActions) =
        div [] [
            p [] [ text "Canceled" ]
            p [] [ Doc.Button "Restart" [] actions.Restart ]
        ]    

    [<SPAEntryPoint>]
    let Main () =
        CombinedFlow()
        |> Flow.EmbedWithCancel Cancelled
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
