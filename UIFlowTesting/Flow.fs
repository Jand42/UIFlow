namespace UIFlowTesting

open System
open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Client

type FlowActions<'A> =
    {
        back: unit -> unit
        cancel: unit -> unit
        next: 'A -> unit
    }
    [<Inline>] member this.Back() = this.back()
    [<Inline>] member this.Cancel() = this.cancel()
    [<Inline>] member this.Next v = this.next v

type WeakRef<'A> [<Inline "new WeakRef($x)">] (x: 'A) =
    [<Inline "$this.deref()">] 
    member this.Deref() = X<'A>  

type NavigateFlow =
    {
        Get: unit -> int
        Set: int -> unit
    }

[<JavaScript>]
module FlowRouting =
    let flowVars = ResizeArray<WeakRef<NavigateFlow>>()

    let flowStateName = "WSUIFlow" + (As<string> DateTime.Now)
    let flowPrevStateName = flowStateName + "Prev"
    
    let markState() =
        let mutable st = JS.Window.History.State
        if st = null || JS.TypeOf st <> JS.Kind.Object then 
            st <- New []
        st?(flowStateName) <- 
            flowVars |> Seq.map (fun var -> 
                let v = var.Deref()
                if v ==. JS.Undefined then
                    JS.Undefined
                else
                    v.Get()
            ) |> Array.ofSeq
        Console.Log("Marked", st)
        JS.Window.History.ReplaceState(st, "")

    let install (nav: NavigateFlow) =
        if flowVars.Count = 0 then
            let handlePopState (e: Dom.Event) =
                let st = e?state
                Console.Log("Popped", st)
                let flowSt =
                    if st <> null && JS.TypeOf st = JS.Kind.Object then
                        st?(flowStateName)
                    else
                        [||] : int[]
                if flowSt !=. JS.Undefined then
                    flowSt |> Array.iteri (fun i p ->
                        if i !=. JS.Undefined then
                            let navI = flowVars[i].Deref() 
                            navI.Set p
                    )
            JS.Window.AddEventListener("popstate", handlePopState, false)
        flowVars.Add(WeakRef(nav))
        flowVars.Count - 1

    let markPrev (index: int) =
        let mutable st = JS.Window.History.State
        if st = null || JS.TypeOf st <> JS.Kind.Object then 
            st <- New []
        st?(flowPrevStateName) <- index
        JS.Window.History.ReplaceState(st, "")

    let tryBack (index: int) =
        let mutable st = JS.Window.History.State
        let indexSt =
            if st <> null && JS.TypeOf st = JS.Kind.Object then
                let prevSt = st?(flowPrevStateName)
                if prevSt !=. JS.Undefined then
                    As<int> prevSt
                else
                    -1
            else
                -1
        if index = indexSt then
            Console.Log "Calling history.back"
            JS.Window.History.Back()
            true
        else
            false

type FlowPage =
    {
        Doc : Doc
        Validate : unit -> bool
    }

[<JavaScript>]
type FlowState =
    {
        mutable Id : int
        Index : Var<int>
        Pages : ResizeArray<Doc>
        mutable EndedOn : int option
        mutable FirstRender : bool
        RenderFirst : unit -> unit
    }

    member this.UpdatePage f =
        this.Index.Update f
        if not this.FirstRender then
            Console.Log("PushState")
            JS.Window.History.PushState(New [], "")
            FlowRouting.markState()
        else
            this.FirstRender <- false

    member this.Add(page) =
        this.UpdatePage(fun i -> 
            this.Pages.Add(page)
            i + 1
        )

    member this.Cancel(page) =
        this.UpdatePage(fun _ ->
            this.Pages.Clear()
            this.Pages.Add(page)
            this.EndedOn <- Some 0
            0
        )

    member this.Navigator =
        {
            Get = fun () -> this.Index.Value
            Set =
                fun res ->
                    if this.Index.Value <> res then
                        this.Index.Set res
        }

    member this.End(page) =
        this.Add(page)
        let endIndex = this.Pages.Count - 1
        for i = 0 to endIndex - 1 do
            this.Pages[i] <- Doc.Empty
        this.EndedOn <- Some endIndex

    member this.Embed() =
        this.Index.View.Map(fun i ->
            // do not navigate away from ending page
            match this.EndedOn with 
            | Some e -> this.Pages[e] 
            | _ ->
                // check if st.Pages contains index i
                if this.Pages.Count >= i + 1 then
                    this.Pages[i]
                elif this.Pages.Count > 1 then
                    // move to last rendered page instead
                    this.UpdatePage(fun _ -> this.Pages.Count - 1)
                    Doc.Empty   
                else
                    this.EndedOn <- None
                    this.Pages.Clear()
                    this.Pages.Add(Doc.Empty)
                    this.UpdatePage(fun _ -> 0)
                    this.RenderFirst()
                    Doc.Empty
        )
        |> Doc.EmbedView

type EndedFlowActions =
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
[<Sealed>]
type Flow =

    static member Map (f: 'A -> 'B) (x: Flow<'A>) =
        Flow(fun st actions -> 
            let mappedActions =
                {
                    back = actions.Back
                    cancel = actions.Cancel
                    next = fun x -> actions.next (f x)
                }
            x.Render st mappedActions
        )

    //static member BindView (m: Flow<View<'A>>) (k: View<'A> -> Flow<'B>) =
    //    Flow(fun st actions ->
    //        let outerActions =
    //            {
    //                back = actions.Back
    //                cancel = actions.Cancel
    //                next = fun res ->
    //                    // check if st.Pages does not contain index i + 1
    //                    if st.Pages.Count <= st.Index.Value + 1 then
    //                        (k res).Render st actions
    //                    else
    //                        st.UpdatePage (fun i ->
    //                            i + 1                       
    //                        )
    //                    FlowRouting.markPrev st.Id
    //            }
    //        m.Render st outerActions    
    //    )

    //static member BindVar (m: Flow<Var<'A>>) (k: Var<'A> -> Flow<'B>) =
    //    Flow(fun st actions ->
    //        let outerActions =
    //            {
    //                back = actions.Back
    //                cancel = actions.Cancel
    //                next = fun res ->
    //                    // check if st.Pages does not contain index i + 1
    //                    if st.Pages.Count <= st.Index.Value + 1 then
    //                        (k res).Render st actions
    //                    else
    //                        st.UpdatePage (fun i ->
    //                            i + 1                       
    //                        )
    //                    FlowRouting.markPrev st.Id
    //            }
    //        m.Render st outerActions    
    //    )

    static member Bind (m: Flow<'A>) (k: 'A -> Flow<'B>) =
        Flow(fun st actions ->
            let outerActions =
                {
                    back = actions.Back
                    cancel = actions.Cancel
                    next = fun res ->
                        // check if st.Pages does not contain index i + 1
                        if st.Pages.Count <= st.Index.Value + 1 then
                            (k res).Render st actions
                        else
                            st.UpdatePage (fun i ->
                                i + 1                       
                            )
                        FlowRouting.markPrev st.Id
                }
            m.Render st outerActions    
        )

    static member View (f: Flow<'A>) =
        Flow(fun st actions -> 
            let resv = Var.Create JS.Undefined
            let viewActions =
                {
                    back = actions.Back
                    cancel = actions.Cancel
                    next = 
                        fun res -> 
                            resv.Set res
                            actions.next resv.View
                }
            f.Render st viewActions
        )

    static member Return x =
        Flow(fun st actions -> actions.Next x)

    static member Embed (fl: Flow<'A>) =
        let mutable renderFirst = ignore
        let var = Var.Create 0
        let st =
            {
                Id = 0
                Index = var
                Pages = ResizeArray [ Doc.Empty ]
                EndedOn = None
                FirstRender = true
                RenderFirst = fun () -> renderFirst ()
            }
        st.Id <- FlowRouting.install st.Navigator
        let action =
            {
                back =
                    fun () -> 
                        if not (FlowRouting.tryBack st.Id) then
                            st.UpdatePage(fun i -> if i > 1 then i - 1 else i)
                next = ignore
                cancel = ignore
            }
        renderFirst <- 
            fun () -> 
                fl.Render st action
                FlowRouting.markState()
        var.Set 0
        st.RenderFirst()
        st.Embed()

    static member EmbedWithCancel (cancel: EndedFlowActions -> Doc) (fl: Flow<'A>) =
        let mutable renderFirst = ignore
        let var = Var.Create 0
        let st =
            {
                Id = 0
                Index = var
                Pages = ResizeArray [ Doc.Empty ]
                EndedOn = None
                FirstRender = true
                RenderFirst = fun () -> renderFirst ()
            }
        st.Id <- FlowRouting.install st.Navigator
        let mutable action = Unchecked.defaultof<FlowActions<'A>>
        let cancelledAction =
            {
                restart = 
                    fun () -> 
                        st.EndedOn <- None
                        fl.Render st action
            }
        action <-
            {
                back =
                    fun () -> 
                        if not (FlowRouting.tryBack st.Id) then
                            st.UpdatePage(fun i -> if i > 1 then i - 1 else i)
                next = ignore
                cancel = fun () -> st.Cancel (cancel cancelledAction)
            }
        renderFirst <- 
            fun () -> 
                fl.Render st action
                FlowRouting.markState()
        var.Set 0
        st.RenderFirst()
        st.Embed()

    [<Inline>]
    static member Define (f: FlowActions<'A> -> Doc) =
        Flow(f)

    static member End doc : Flow<unit> =
        Flow(fun st _ -> st.End doc)

    //static member EndRestartable (f: EndedFlowActions -> Doc) : Flow<unit> =
    //    Flow(fun st _ -> st.End doc)

[<JavaScript>]
[<Sealed>]
type FlowBuilder() =
    //[<Inline>] member x.Bind(comp: Flow<Var<'A>>, func: Var<'A> -> Flow<'B>) = Flow.BindVar comp func
    //[<Inline>] member x.Bind(comp: Flow<View<'A>>, func: View<'A> -> Flow<'B>) = Flow.BindView comp func
    [<Inline>] member x.Bind(comp: Flow<'A>, func: 'A -> Flow<'B>) = Flow.Bind comp func
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

    //[<Extension; Inline>]
    //static member Bind(flow: Flow<'A>, f: Func<View<'A>, Flow<'B>>) =
    //    Flow.Bind(flow, f.Invoke)

    [<Extension; Inline>]
    static member Embed(flow) =
        Flow.Embed flow

    [<Extension; Inline>]
    static member EmbedWithCancel(flow, cancel: Func<EndedFlowActions, Doc>) =
        Flow.EmbedWithCancel cancel.Invoke flow
