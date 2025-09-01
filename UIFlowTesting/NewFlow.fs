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

[<JavaScript>]
type FlowState =
    {
        Index: Var<int>
        Pages: ResizeArray<Doc>
        mutable EndedOn: int option
        RenderFirst: unit -> unit
    }

    member this.Add(page) =
        this.Index.Update(fun i -> 
            this.Pages.Add(page)
            i + 1
        )

    member this.Cancel(page) =
        this.Index.Update(fun _ ->
            this.Pages.Clear()
            this.Pages.Add(page)
            0
        )

    member this.End(page) =
        this.Add(page)
        let endIndex = this.Pages.Count - 1
        for i = 0 to endIndex - 1 do
            this.Pages[i] <- Doc.Empty
        this.EndedOn <- Some endIndex

    member this.Embed() =
        this.Index.View.Map(fun i ->
            JavaScript.Console.Log("Flow embed page", i)
            let reset() =
                this.EndedOn <- None
                this.Pages.Clear()
                this.Pages.Add(Doc.Empty)
                this.Index.Set(0)
                this.RenderFirst()
                Doc.Empty
            // if navigated away from ending page, reset
            match this.EndedOn with 
            | Some e when e <> i -> reset()
            | _ ->
                // check if st.Pages contains index i
                if this.Pages.Count >= i + 1 then
                    this.Pages[i]
                elif this.Pages.Count > 1 then
                    // show last rendered page instead
                    this.Pages[i]       
                else
                    reset()
        )
        |> Doc.EmbedView

type CancelledFlowActions =
    {
        restart: unit -> unit
    }
    [<Inline>] member this.Restart() = this.restart()

[<JavaScript>]
[<Sealed>]
type Flow<'A>(render: FlowState -> FlowActions<'A> -> unit) =
        
    new (define: Func<FlowActions<'A>, Doc>) =
        Flow(fun st actions -> st.Add (define.Invoke actions))

    [<Inline>] member this.Render = render

[<JavaScript>]
type Flow =

    static member Map (f: 'A -> 'B) (x: Flow<'A>) =
        Flow(fun st actions -> 
            let mappedActions =
                {
                    back = actions.Back
                    cancel = actions.Cancel
                    next = fun x -> actions.Next (View.Map f x)
                }
            x.Render st mappedActions
        )

    static member Bind (m: Flow<'A>) (k: View<'A> -> Flow<'B>) =
        Flow(fun st actions ->
            let outerActions =
                {
                    back = actions.Back
                    cancel = actions.Cancel
                    next = fun resView ->
                        let i = st.Index.Value
                        // check if st.Pages does not contain index i + 1
                        if st.Pages.Count < i + 2 then
                            (k resView).Render st actions
                        st.Index.Set(i + 1)
                }
            m.Render st outerActions    
        )

    static member Return x =
        Flow(fun st actions -> actions.Next x)

    static member EmbedWithRouting (var: Var<int>) (fl: Flow<'A>) =
        let mutable renderFirst = ignore
        let st =
            {
                Index = var
                Pages = ResizeArray [ Doc.Empty ]
                EndedOn = None
                RenderFirst = fun () -> renderFirst ()
            }
        let action =
            {
                back = fun () -> st.Index.Update(fun i -> if i > 1 then i - 1 else i)
                next = ignore
                cancel = ignore
            }
        renderFirst <- fun () -> fl.Render st action
        var.Set 0
        st.RenderFirst()
        st.Embed()

    static member EmbedWithRoutingAndCancel (var: Var<int>) (cancel: CancelledFlowActions -> Doc) (fl: Flow<'A>) =
        let mutable renderFirst = ignore
        let st =
            {
                Index = var
                Pages = ResizeArray [ Doc.Empty ]
                EndedOn = None
                RenderFirst = fun () -> renderFirst ()
            }
        let mutable action = Unchecked.defaultof<FlowActions<'A>>
        let cancelledAction =
            {
                restart = fun () -> fl.Render st action
            }
        action <-
            {
                back = fun () -> st.Index.Update(fun i -> if i > 1 then i - 1 else i)
                next = ignore
                cancel = fun () -> st.Cancel (cancel cancelledAction)
            }
        renderFirst <- fun () -> fl.Render st action
        var.Set 0
        st.RenderFirst()
        st.Embed()

    static member Embed (fl: Flow<'A>) =
        Flow.EmbedWithRouting (Var.Create 0) fl        

    static member EmbedWithCancel (cancel: CancelledFlowActions -> Doc) (fl: Flow<'A>) =
        Flow.EmbedWithRoutingAndCancel (Var.Create 0) cancel fl        

    [<Inline>]
    static member Define (f: FlowActions<'A> -> Doc) =
        Flow(f)

    static member End doc =
        Flow(fun st actions -> st.End doc)

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
