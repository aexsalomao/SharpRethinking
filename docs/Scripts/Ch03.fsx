(**
---
title: Chapter 3
category: Scripts
categoryindex: 1
index: 3
---
*)

#load "Common.fsx"
#r "nuget: FSharp.Stats, 0.4.6"
#r "nuget: Plotly.NET, 3.0.0"
#r "nuget: Plotly.NET.Interactive, 3.0.0"
#r "nuget: Numpy, 3.10.1.29"
#r "nuget: Microsoft.Solver.Foundation, 3.1.0"
#r "nuget: MathNet.Numerics, 5.0.0"

open System
open Common
open Common.NumpyUtils
open FSharp.Stats
open Plotly.NET
open Numpy
open MathNet.Numerics
open MathNet.Numerics.Distributions
open Plotly.NET.LayoutObjects

(**
**Reconstructing the posterior distribution via sampling.**

**Estimate posterior distribution with the Grid Approximation method**
    - Choose n points to estimate
    - Compute the likelihoods over a range of probabilities (parameter values) (binomial distribution)
    - Adjust for the given prior (uniform, heavyside, etc ...)
    - Standardize
    - The resulting posterior distribution of parameter values will have a posterior probability associated to each value.
    - Perform a weighted sampling from this posterior distribution.
*)

let samplingVsGridApproximationFig = 
    let nTrials = 3
    let nSuccesses = 3
    let probas = Generate.LinearSpaced(1000, 0., 1.)
    let prior = [|for _ in 0. .. ((float probas.Length) - 1.) -> 1.|]
    let likelihood = [|for p in probas -> Binomial.PMF(p=p, n=nTrials, k=nSuccesses)|]
    let posterior = rethinking.computePosterior likelihood prior

    [100; 1000; 10000; 100000]
    |> List.map (fun xs -> 
        np.random.choice(a=toNDarray probas, size=[|xs|], p=toNDarray posterior) 
        |> toSharp
        |> fun xs ->
            let traceName = $"# Samples: {xs.Length}"
            Chart.Histogram(xs, Name=traceName))
    |> Chart.combine
    |> Chart.withXAxisStyle("Proportion of water (parameter p)", MinMax=(0., 1.))
    |> Chart.withYAxisStyle("Counts")
    |> fun samplingFig ->
        [samplingFig; 
         rethinking.binomialMultiGridApproximationFig (fun x -> 1.) [probas.Length] nTrials nSuccesses]
        |> Chart.Grid(2, 1)
        |> Chart.withSize(1500, 750)
        |> Chart.withTitle($"Binomial Distribution (nTrials: {nTrials}, nSucesses: {nSuccesses})")

(**
"All you have done so far is crudely replicate the posterior density you had already computed. That isn't of much value."

1. Intervals of defined boundaries
2. Intervals of defined probability mass
3. Point estimates
*)

// Intervals of defined boundaries
let probabilityMass parameterValues probabilities lowerBound upperBound : float = 
    Seq.zip parameterValues probabilities
    |> Seq.sumBy (fun (param, proba) -> 
        if param > lowerBound && param < upperBound 
        then proba else 0.)

let probabilityMassByCounts parameterValues lowerBound upperBound : float = 
    let x = 1. / float (Seq.length parameterValues)
    parameterValues
    |> Seq.sumBy (fun param -> 
        if param > lowerBound && param < upperBound
        then x else 0.)

// Intervals of defined mass
let percentileInterval q data : float * float = 
    let tailMass = (1. - q) / 2.
    [|tailMass; 1. - tailMass|]
    |> Array.map (fun q -> Quantile.normal q data)
    |> fun xs -> xs.[0], xs.[1]

let formatNumb (x : float) = Math.Round(x, 2).ToString("0.00")

let intervalFig parameterGrid probabilities bounds =
    let computeProbaMassByBounds lowerBound upperBound = probabilityMass parameterGrid probabilities lowerBound upperBound
    
    let name = 
        $"""Interval: [{bounds |> fst |> formatNumb} - {bounds |> snd |> formatNumb}]""" + 
        $" Probability mass:" +
        $""" [ L: {computeProbaMassByBounds (parameterGrid |> Seq.min) (bounds |> fst) |> formatNumb}""" +
        $""" | C: {computeProbaMassByBounds (bounds |> fst) (bounds |> snd) |> formatNumb}""" +
        $""" | R: {computeProbaMassByBounds (bounds |> snd) (parameterGrid |> Seq.max) |> formatNumb}]"""
    
    let xy = Array.zip parameterGrid probabilities
    [
        xy
        |> Array.filter (fun (param, proba) -> param >= fst bounds && param <= snd bounds)
        |> Array.unzip
        |> fun (x, y) -> Chart.SplineArea(x, y, Name=name, MarkerColor=Color.fromString("purple"));

        Chart.Line(xy, ShowLegend=false, MarkerColor=Color.fromString("purple"))
    ]
    |> Chart.combine
    |> Chart.withTitle("Interval comparison")
    |> Chart.withXAxisStyle("Proportion of water (parameter p)", MinMax=(0., 1.))
    |> Chart.withYAxisStyle("Posterior probability")

let nTrials = 3
let nSuccesses = 3
let paramGrid = Generate.LinearSpaced(1000, 0., 1.)
let prior = [|for _ in 0. .. ((float paramGrid.Length) - 1.) -> 1.|]
let likelihood = [|for p in paramGrid -> Binomial.PMF(p=p, n=nTrials, k=nSuccesses)|]
let posterior = rethinking.computePosterior likelihood prior
let samples = np.random.choice(a=toNDarray paramGrid, size=[|1000000|], replace=true, p=toNDarray posterior) |> toSharp

let intervalComparisonFig = 
    [
        intervalFig paramGrid posterior (0., 0.5); 
        intervalFig paramGrid posterior (0.5, 0.75);
        intervalFig paramGrid posterior (0., Quantile.normal 0.8 samples);
        intervalFig paramGrid posterior (percentileInterval 0.8 samples)
    ]
    |> Chart.Grid(2, 2)
    |> Chart.withSize(1500, 800)

(**
HPDI - Highest Posterior Desity Interval
*)

(**
Point estimates

Given the entire posterior distribution, what value should you report?

*Maximum a posteriori* (MAP) estimate.
-> The mode of the posterior distribution
*)

let modeFromGridEstimate = 
    Seq.zip paramGrid posterior
    |> Seq.sortByDescending snd
    |> Seq.head
    |> fst

let computeMode x = 
    x
    |> Seq.groupBy id
    |> Seq.map (fun (paramValue, xs) -> (paramValue, Seq.length xs))
    |> Seq.sortByDescending snd
    |> Seq.head
    |> fst

let pointEstimatesPlot paramGrid posterior = 
    let initXAxisLine x = 
        Shape.init (StyleParam.ShapeType.Line, X0 = x, X1 = x, Y0 = 0, Y1 = 1, Xref = "x", Yref = "paper", Line = Line.init (Width = 1.5))

    let initAnnotation x text =
        Annotation.init (X = x - (x * 0.025), Y = 0.15, Text = text, XRef = "x", YRef = "paper", ShowArrow = false, TextAngle = 270.)

    let mean, median, mode = 
        let samples = 
            np.random.choice(a=toNDarray paramGrid, size=[|1000000|], replace=true, p=toNDarray posterior) 
            |> toSharp
        
        Seq.average samples,
        Seq.median samples,
        computeMode samples

    let chartName = $"Posterior Probabilities (Mean: {formatNumb mean} ; Median: {formatNumb median} ; Mode: {formatNumb mode})"

    Chart.Line(Array.zip paramGrid posterior, Name=chartName)
    |> Chart.withAnnotations ([ initAnnotation mean "mean"; initAnnotation median "median"; initAnnotation mode "mode"])
    |> Chart.withShapes([initXAxisLine mean; initXAxisLine median; initXAxisLine mode])
    |> Chart.withXAxisStyle("Proportion of water (parameter p)")
    |> Chart.withYAxisStyle("Posterior probabilities")
    |> Chart.withSize(600., 500.)

// Loss functions
// Loss functions imply different point estimates

type LossFunc = 
    | ApproximateZeroOne 
    | ZeroOne
    | Absolute
    | Quadratic
    override this.ToString() = 
        match this with
        | ApproximateZeroOne -> "Approximate Zero One Loss"
        | ZeroOne -> "L0 - Zero One Loss"
        | Absolute -> "L1 - Absolute Loss"
        | Quadratic -> "L2 - Quadratic Loss"

let pickLossError lossError theta theta' : float = 
    match lossError with
    | ApproximateZeroOne -> if theta - theta' < 1e-16 then 0. else 1.
    | ZeroOne -> if theta.Equals(theta') then 0. else 1.
    | Absolute -> (theta - theta') |> abs
    | Quadratic -> (theta - theta') ** 2.

let expectedLoss lossFunc (paramGuess : float) (paramGrid : float array) posterior : float =
    Array.zip paramGrid posterior
    |> Array.sumBy (fun (trueParam, posteriorProba) -> posteriorProba * lossFunc trueParam paramGuess)

let lossFuncFig lossError paramGrid posterior = 
    let lossFuncXy = 
        Generate.LinearSpaced(10000, 0., 1.)
        |> Array.map( fun xs -> (xs, expectedLoss (pickLossError lossError) xs paramGrid posterior))
    
    let minX, minY = 
        lossFuncXy 
        |> Array.minBy snd

    let minXYString =
        $"Minimum: ({formatNumb minX} ; {formatNumb minY})"

    [Chart.Line(lossFuncXy, Name=lossError.ToString()); Chart.Point([|(minX, minY)|], Name=minXYString, MarkerSymbol=StyleParam.MarkerSymbol.StarDiamond)
    ]
    |> Chart.combine
    |> Chart.withXAxisStyle("Proportion of water (parameter p)")
    |> Chart.withYAxisStyle("Loss")

let pointEstimateAndlossFuncComparisonFig = 
    let chartTitle = $"Point estimates and Loss functions (n = {Seq.length paramGrid}; Grid Aprroximation)"
    [
        pointEstimatesPlot paramGrid posterior;
        lossFuncFig LossFunc.ZeroOne paramGrid posterior;
        [
            lossFuncFig LossFunc.ApproximateZeroOne paramGrid posterior;
            lossFuncFig LossFunc.Absolute paramGrid posterior; 
            lossFuncFig LossFunc.Quadratic paramGrid posterior
        ]
        |> Chart.combine

    ]
    |> Chart.Grid(1, 3)
    |> Chart.withSize(1800., 500.)
    |> Chart.withTitle(chartTitle)

(**
Sampling to simulate prediction
*)