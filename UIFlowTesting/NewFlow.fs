namespace UIFlowTesting

open WebSharper
open WebSharper.UI
open WebSharper.UI.Client

type FlowActions<'A> =
    {
        Back: unit -> unit
        Cancel: unit -> unit
        Next: View<'A> -> unit
    }

type CancelledFlowActions =
    {
        Restart: unit -> unit
    }

[<JavaScript>]
[<Sealed>]
type Flow<'A>(render: Var<Doc> -> FlowActions<'A> -> unit) =
        
    member this.Render = render

[<JavaScript>]
type Flow =

    static member Map (f: 'A -> 'B) (x: Flow<'A>) =
        Flow(fun var actions -> 
            let mappedActions =
                {
                    Back = actions.Back
                    Cancel = actions.Cancel
                    Next = fun x -> actions.Next (View.Map f x)
                }
            x.Render var mappedActions
        )

    static member Bind (m: Flow<'A>) (k: View<'A> -> Flow<'B>) =
            
        Flow(fun var combinedActions ->
            let next = ref None
            let outerActions =
                {
                    Back = combinedActions.Back
                    Cancel = combinedActions.Cancel
                    Next = fun resView ->
                        match next.Value with
                        | Some existing -> var.Set existing
                        | _ ->
                            let current = var.Value
                            let innerActions = 
                                { combinedActions with
                                    Back = fun () -> var.Set current
                                }
                            (k resView).Render var innerActions
                            next.Value <- Some var.Value
                }
            m.Render var outerActions    
        )

    static member Return x =
        Flow(fun var actions -> actions.Next x)

    static member Embed (fl: Flow<'A>) =
        let var = Var.Create Doc.Empty
        let action =
            {
                Back = ignore
                Next = ignore
                Cancel = ignore
            }
        fl.Render var action
        Doc.EmbedView var.View 

    static member EmbedWithCancel (cancel: CancelledFlowActions -> Doc) (fl: Flow<'A>) =
        let var = Var.Create Doc.Empty
        let mutable action = Unchecked.defaultof<FlowActions<'A>>
        let cancelledAction =
            {
                Restart = fun () -> fl.Render var action
            }
        action <-
            {
                Back = ignore
                Next = ignore
                Cancel = fun () -> var.Set (cancel cancelledAction)
            }
        fl.Render var action
        Doc.EmbedView var.View 

    static member Define (f: FlowActions<'A> -> Doc) =
        Flow(fun var actions -> var.Set (f actions))

    static member Static doc =
        Flow(fun var actions -> var.Set doc; actions.Next (View.Const ()))

[<JavaScript>]
[<Sealed>]
type FlowBuilder() =
    member x.Bind(comp, func) = Flow.Bind comp func
    member x.Return(value) = Flow.Return value
    member x.ReturnFrom(inner: Flow<'A>) = inner

[<JavaScript>]
[<AutoOpen>]
module FlowHelper =
    let flow = FlowBuilder()

