(**
---
title: Chapter 2: Small Worlds and Large Worlds
category: Practice
categoryindex: 1
index: 2
---
*)

open System
open System.IO
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

(**
### Nuget packages
*)

#r "nuget: Plotly.NET, 3.0.0"
#r "nuget: Plotly.NET.Interactive, 3.0.2"
#r "nuget: MathNet.Numerics, 5.0.0"
#r "nuget: Numpy, 3.10.1.29"

open Plotly.NET
open Plotly.NET.Interactive
open MathNet.Numerics
open MathNet.Numerics.Distributions
open Numpy

(**
### Scripts
*)

#load "Common.fsx"

open Common
open Common.GridApproximation

(**
# Practice

Problems are labeled Easy(E), Medium(M), and Hard(H).
*)

(**
2E1. Which of the expressions below correspond to the statement: *the probability of rain on Monday* ?

(4) $Pr(rain|Monday)$
*)

(**
2E2. Which of the following statements corresponds to the expression: $Pr(rain|Monday)$ ?

(3) The probability of rain, given that it is Monday.
*)

(**
2E3. Pr(Monday|rain); Pr(rain|Monday)Pr(Monday) / Pr(rain)
*)

(**
2E4. "the probability of water is 0.7" -> Here, 0.7 represents the proportion of the globe that is covered in water.
*)

(**
Medium
*)

(**
2M1. Recall the globe tossing model from the chapter. Compute and plot the grid approximate
posterior distribution for each of the following sets of observations.In each case, assume a uniform
prior for p.
(1) W,W,W
(2) W,W,W,L
(3) L,W,W,L,W,W,W
*)
let binomialPosteriorBreakdownFig priorFunc nTrials nSucesses = 
    let probas = Generate.LinearSpaced(10000, 0., 1.)
    let prior = probas |> Array.map priorFunc
    let likelihood = probas |> Array.map (fun xs -> Binomial.PMF(p=xs, n=nTrials, k=nSucesses))

    [Chart.Line(probas, prior, LineDash=StyleParam.DrawingStyle.Dot, Name="Prior");
     Chart.Line(probas, likelihood, LineDash=StyleParam.DrawingStyle.Dot, Name="Likelihood"); 
     Chart.Line(probas, GridApproximation.posteriorProbabilities prior likelihood, Name="Posterior")]
    |> Chart.Grid(1, 3)
    |> Chart.withSize(1050, 400)

let multiBinominalPosteriorFig priorFunc points nTrials nSuccesses = 
    let title = $"Grid approximation - Posterior Distribution (n={nTrials}, k={nSuccesses})"
    points
    |> Seq.map (fun xs -> 
        let probas = Generate.LinearSpaced(xs, 0., 1.)
        let prior = probas|> Array.map priorFunc
        Chart.Line(probas, binomialPosterior prior nTrials nSuccesses, Name = $"# Grid Points: {probas.Length}"))
    |> Chart.combine
    |> Chart.withTitle(title)
    |> Chart.withXAxisStyle("Parameter p", MinMax=(0., 1.))
    |> Chart.withYAxisStyle("Posterior probability")
    |> Chart.withSize(1000., 500.)

(**
### 2M1. (W,W,W) - Uniform Prior | N=3, W=3, L=0
*)
(***do-not-eval***)
binomialPosteriorBreakdownFig (fun x -> 1.) 3 3
(***hide***)
binomialPosteriorBreakdownFig (fun x -> 1.) 3 3
|> GenericChart.toChartHTML
(*** include-it-raw ***)

(**
### 2M1. (W,W,W) - Heavyside step function Prior | N=3, W=3, L=0
*)
(***do-not-eval***)
binomialPosteriorBreakdownFig (fun x -> if x < 0.5 then 0. else 1.) 3 3
(***hide***)
binomialPosteriorBreakdownFig (fun x -> if x < 0.5 then 0. else 1.) 3 3
|> GenericChart.toChartHTML
(*** include-it-raw ***)

(**
### 2M1. (W,W,W) - Heavyside step function Peeeeeerior | N=3, W=3, L=0
*)
(***do-not-eval***)
binomialPosteriorBreakdownFig (fun x -> if x <= 0.5 then x ** 2. else (x - 1.) **2 ) 3 3
(***hide***)
[binomialPosteriorBreakdownFig (fun x -> 1.) 3 3
 binomialPosteriorBreakdownFig (fun x -> if x < 0.5 then 0. else 1.) 3 3
 binomialPosteriorBreakdownFig (fun x -> if x <= 0.5 then x ** 2. else (x - 1.) **2 ) 3 3
]
|> Chart.Grid(1, 3)
|> GenericChart.toChartHTML
(*** include-it-raw ***)

(**
### 2M1. (W,W,W) - Heavyside step function Prior | N=3, W=3, L=0
*)
(***do-not-eval***)
multiBinominalPosteriorFig (fun x -> 1.) [10 .. 10 .. 100] 3 3
(***hide***)
multiBinominalPosteriorFig (fun x -> 1.) [10 .. 10 .. 100] 3 3
|> GenericChart.toChartHTML
(*** include-it-raw ***)

// (2) W,W,W,L
// N = 4; W = 3; L = 1
multiBinominalPosteriorFig (fun x -> 1.) [10 .. 10 .. 100] 4 3

// (3) L,W,W,L,W,W,W
// N = 7; W = 5; L = 2
multiBinominalPosteriorFig (fun x -> 1.) [10 .. 10 .. 100] 10 1

(**
2M2. Now assume a prior for p that is equal to zero when p < 0.5 and is a positive constant when
p ≥ 0.5. Again compute and plot the grid approximate posterior distribution for each of the sets of
observations in the problem just above.
*)

// (1) W,W,W
// N = 3; W = 3 ; L = 0
multiBinominalPosteriorFig (fun x -> if x < 0.5 then 0. else 1.) [10 .. 10 .. 100] 3 3

// (2) W,W,W,L
// N = 4; W = 3; L = 1
multiBinominalPosteriorFig (fun x -> if x < 0.5 then 0. else 1.) [10 .. 10 .. 100] 4 3

// (3) L,W,W,L,W,W,W
// N = 7; W = 5; L = 2
multiBinominalPosteriorFig (fun x -> if  x < 0.5 then 0. else 1.) [10 .. 10 .. 100] 7 5 

(**
testing
*)

(**
2M3. 
Suppose there are two globes, one for Earth and one for Mars. 
The Earth globe is 70% covered in water. The Mars globe is 100% land. 
Further suppose that one of these globes—you don’t know which—was tossed in the air and produced a “land” observation. 
Assume that each globe was equally likely to be tossed. Show that the posterior probability that the globe was the Earth, 
conditional on seeing “land” (Pr(Earth|land)), is 0.23.

Pr(Earth|Land) = 0.23
Pr(Earth|Land) = Pr(Land|Earth) * Pr(Earth) / Pr(Land)
*)

let prLandGivenEarth = 0.3
let prLandGivenMars = 1.
let prEarth = 0.5
let prMars = 0.5

let prLand = (prMars * prLandGivenMars) + (prEarth * prLandGivenEarth)
let prEarthGivenLand = (prLandGivenEarth * prEarth) / (prLand)

(**
2M4.

3 Cards - X, Y, Z
2 Sides - B, W

BB, BW, WW
*)