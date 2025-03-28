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

        let FormsPage() =
            IndexTemplate.FormsPage()
                .Doc()

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
            )
            |> Doc.EmbedView

        IndexTemplate()
            .Content(renderInnerPage currentPage)
            .SwitchToHome(fun _ ->
                currentPage := Home
            )
            .Bind()
