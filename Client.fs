namespace MySPA01

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Templating
open WebSharper.Sitelets

[<JavaScript>]
module Client =
    open WebSharper.UI
    open WebSharper.UI.Notation

    // Our SPA endpoints
    type EndPoint =
        | [<EndPoint "/">] Home
        | [<EndPoint "/charting">] Charting
        | [<EndPoint "/forms">] Forms
        | [<EndPoint "/counter">] Counter
        | [<EndPoint "/calculator">] Calculator

    // The templates are loaded from the DOM, so you just can edit index.html
    // and refresh your browser, no need to recompile unless you add or remove holes.
    type IndexTemplate = Template<"wwwroot/index.html", ClientLoad.FromDocument>

    module Pages =
        open WebSharper.UI.Html
        open WebSharper.Charting

        module Home =
            let People =
                ListModel.FromSeq [
                    "John"
                    "Paul"
                ]

            let NewName = Var.Create ""

        let HomePage() =
            IndexTemplate.HomePage()
                .ListContainer(
                    Home.People.View.DocSeqCached(fun (name: string) ->
                        IndexTemplate.ListItem().Name(name).Doc()
                    )
                )
                .Name(Home.NewName)
                .Add(fun e ->
                    Home.People.Add(Home.NewName.Value)
                    Home.NewName.Value <- ""
                )
                .Doc()

        let ChartPage() =
            let labels =
                [| "Eating"; "Drinking"; "Sleeping";
                   "Designing"; "Coding"; "Cycling"; "Running" |]
            let dataset1 = [|28.0; 48.0; 40.0; 19.0; 96.0; 27.0; 100.0|]
            let dataset2 = [|65.0; 59.0; 90.0; 81.0; 56.0; 55.0; 40.0|]
    
            let chart =
                Chart.Combine [
                    Chart.Radar(Array.zip labels dataset1)
                        .WithFillColor(Color.Rgba(151, 187, 205, 0.2))
                        .WithStrokeColor(Color.Name "blue")
                        .WithPointColor(Color.Name "darkblue")

                    Chart.Radar(Array.zip labels dataset2)
                        .WithFillColor(Color.Rgba(220, 220, 220, 0.2))
                        .WithStrokeColor(Color.Name "green")
                        .WithPointColor(Color.Name "darkgreen")
                ]
    
            IndexTemplate.ChartingPage()
                .Chart(Renderers.ChartJs.Render(chart, Size = Size(500, 300)))
                .Doc()

        open WebSharper.Forms

        let FormsPage() =
            let v = Var.Create ""
            Form.Return (fun name email message -> name, email, message)
            <*> (Form.Yield "" |> Validation.IsNotEmpty "Name should be non-empty")
            <*> Form.Yield ""
            <*> Form.Yield ""
            |> Form.WithSubmit
            |> Form.Run (fun (name, email, message) ->
                async {
                    // Call the server
                    //let! out = Server.Echo <| sprintf "From %s(%s): %s" name email message
                    let out = "TODO"
                    // Propagate the result to the UI via `v`
                    v := out
                } |> Async.StartImmediate
            )
            |> Form.Render (fun name email message submitter ->
                IndexTemplate.ContactForm()
                    .Name(name)
                    .Email(email)
                    .Message(message)
                    .Response(v.View)
                    .OnSend(fun e -> submitter.Trigger())
                    .Doc()
            )

        let storage = JS.Window.LocalStorage
        let counter =
            let curr = storage.GetItem "counter"
            if curr = "" then
                0
            else
                int curr
            |> Var.Create

        let CounterPage() =
            IndexTemplate.CounterPage()
                .Value(View.Map string counter.View)
                .Decrement(fun e ->
                    counter := counter.Value - 1
                    storage.SetItem("counter", string counter.Value)
                )
                .Increment(fun e ->
                    counter := counter.Value + 1
                    storage.SetItem("counter", string counter.Value)
                )
                .Doc()

        open Calculator
        open WebSharper.JavaScript

        let line(ctx: CanvasRenderingContext2D, x1, y1, x2, y2) =
            ctx.BeginPath()
            ctx.MoveTo(x1, y1)
            ctx.LineTo(x2, y2)
            ctx.Stroke()

        let evalAt e x =
            let env = ["x", x]
            Evaluator.Eval env e


        let Calculator () =
            Form.Return (fun v from ``to`` formula -> v, from, ``to``, formula)
            <*> (Form.Yield "x" |> Validation.IsNotEmpty "Variable should be non-empty")
            <*> (Form.Yield "-20" |> Validation.IsMatch "[0-9]+" "From should be a number")
            <*> (Form.Yield "20" |> Validation.IsMatch "[0-9]+" "To should be a number")
            <*> (Form.Yield "sin(x*x)*cos(x)" |> Validation.IsNotEmpty "Formula should be non-empty")
            |> Form.WithSubmit
            |> Form.Render (fun v from ``to`` formula submitter ->
                IndexTemplate.Calculator()
                    .Variable(v)
                    .From(from)
                    .To(``to``)
                    .Formula(formula)
                    .OnSend(fun node ->
                        async {
                            node.Vars.Errors := ""
                            let! formula = node.Vars.Formula
                            let! from = node.Vars.From
                            let! ``to`` = node.Vars.To
                            match formula with
                            | Language.Expression (e, Language.Eof) ->
                                let scaleX = 50.
                                let scaleY = -50.
                                let from = float from
                                let ``to`` = float ``to``
                                let ctx = As<HTMLCanvasElement>(node.Anchors.Canvas).GetContext "2d"
                                ctx.ClearRect(0, 0, 1000, 1000)
                                let skip = (``to`` - from) / 1000.
                                [from .. skip .. ``to``]
                                |> Seq.fold (fun (x1,y1) x2 ->
                                    let y2 = evalAt e x2
                                    line(ctx, 
                                        x1*scaleX+500., y1*scaleY+200.,
                                        x2*scaleX+500., y2*scaleY+200.)
                                    (x2, y2)
                                ) (from, evalAt e from)
                                |> ignore
                            | _ ->
                                node.Vars.Errors := "Syntax error in formula, expecting an expression"
                        } |> Async.StartImmediate
                    )
                    .Doc()
            )

    // Create a router for our endpoints
    let router = Router.Infer<EndPoint>()
    // Install our client-side router and track the current page
    let currentPage = Router.InstallHash Home router

    type Router<'T when 'T: equality> with
        member this.LinkHash (ep: 'T) = "#" + this.Link ep

    [<SPAEntryPoint>]
    let Main () =
        let renderInnerPage (currentPage: Var<EndPoint>) =
            currentPage.View.Map (fun endpoint ->
                match endpoint with
                | Home      -> Pages.HomePage()
                | Charting  -> Pages.ChartPage()
                | Forms     -> Pages.FormsPage()
                | Counter   -> Pages.CounterPage()
                | Calculator -> Pages.Calculator()
            )
            |> Doc.EmbedView

        IndexTemplate()
            .Content(renderInnerPage currentPage)
            .SwitchToHome(fun _ ->
                currentPage := Home
            )
            .Bind()
