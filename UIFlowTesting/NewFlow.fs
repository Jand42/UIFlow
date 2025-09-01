namespace UIFlowTesting

open System
open WebSharper
open WebSharper.UI
open WebSharper.UI.Client

type FlowActions<'A> =
    {
        back: unit -> unit
        cancel: unit -> unit
        next: View<'A> -> unit
    }
    [<Inline>] member this.Back() = this.back()
    [<Inline>] member this.Cancel() = this.cancel()
    [<Inline>] member this.Next v = this.next v

type CancelledFlowActions =
    {
        restart: unit -> unit
    }
    [<Inline>] member this.Restart() = this.restart()

[<JavaScript>]
[<Sealed>]
type Flow<'A>(render: Var<Doc> -> FlowActions<'A> -> unit) =
        
    new (define: Func<FlowActions<'A>, Doc>) =
        Flow(fun var actions -> var.Set (define.Invoke actions))

    [<Inline>] member this.Render = render

[<JavaScript>]
type Flow =

    static member Map (f: 'A -> 'B) (x: Flow<'A>) =
        Flow(fun var actions -> 
            let mappedActions =
                {
                    back = actions.Back
                    cancel = actions.Cancel
                    next = fun x -> actions.Next (View.Map f x)
                }
            x.Render var mappedActions
        )

    static member Bind (m: Flow<'A>) (k: View<'A> -> Flow<'B>) =
        Flow(fun var combinedActions ->
            let next = ref None
            let outerActions =
                {
                    back = combinedActions.Back
                    cancel = combinedActions.Cancel
                    next = fun resView ->
                        match next.Value with
                        | Some existing -> var.Set existing
                        | _ ->
                            let current = var.Value
                            let innerActions = 
                                { combinedActions with
                                    back = fun () -> var.Set current
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
                back = ignore
                next = ignore
                cancel = ignore
            }
        fl.Render var action
        Doc.EmbedView var.View 

    static member EmbedWithCancel (cancel: CancelledFlowActions -> Doc) (fl: Flow<'A>) =
        let var = Var.Create Doc.Empty
        let mutable action = Unchecked.defaultof<FlowActions<'A>>
        let cancelledAction =
            {
                restart = fun () -> fl.Render var action
            }
        action <-
            {
                back = ignore
                next = ignore
                cancel = fun () -> var.Set (cancel cancelledAction)
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
    [<Inline>] member x.Bind(comp, func) = Flow.Bind comp func
    [<Inline>] member x.Return(value) = Flow.Return value
    [<Inline>] member x.ReturnFrom(inner: Flow<'A>) = inner

[<JavaScript>]
[<AutoOpen>]
module FlowHelper =
    let flow = FlowBuilder()

open System.Runtime.CompilerServices

[<Extension; Sealed; JavaScript>]
type FlowExtensions =

    [<Extension; Inline>]
    static member Map(flow: Flow<'A>, f: Func<'A, 'B>) =
        Flow.Map f.Invoke flow

    [<Extension; Inline>]
    static member Bind(flow: Flow<'A>, f: Func<View<'A>, Flow<'B>>) =
        Flow.Bind flow f.Invoke

    [<Extension; Inline>]
    static member Embed(flow) =
        Flow.Embed flow

    [<Extension; Inline>]
    static member EmbedWithCancel(flow, cancel: Func<CancelledFlowActions, Doc>) =
        Flow.Embed flow
