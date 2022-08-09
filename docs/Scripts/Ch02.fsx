(**
---
title: Chapter 2
category: Scripts
categoryindex: 1
index: 2
---
*)

#r "nuget: FSharp.Stats, 0.4.6"
#r "nuget: Plotly.NET,2.0.0-preview.17"
#r "nuget: Plotly.NET.Interactive,2.0.0-preview.17"

open FSharp.Stats
open FSharp.Stats.Distributions
open Plotly.NET
open Plotly.NET.Interactive

let computePosterior likelihood prior : list<float> = 
    prior
    |> List.zip likelihood
    |> List.map (fun (likelihood_i, prior_i) -> likelihood_i * prior_i)
    |> fun unstandardizedPosterior -> 
        let sum = List.sum unstandardizedPosterior
        unstandardizedPosterior |> List.map (fun xs -> xs / sum)

let gridApproximationFig (probas : list<float>) (prior : list<float>) (likelihood : list<float>) : GenericChart.GenericChart = 
    [
        Chart.Line(probas, prior, ShowMarkers=true, Name="Prior"); 
        Chart.Line(probas, likelihood, ShowMarkers=true, Name="Likelihood"); 
        Chart.Line(probas, computePosterior prior likelihood, ShowMarkers=true, Name="Posterior")
    ] 
    |> Chart.Grid(1, 3)
    |> Chart.withTitle($"Grid approximation (n: {probas.Length})")
    |> Chart.withYAxisStyle(TitleText="Plausibility")
    |> Chart.withSize(1500.0, 500.0)

// Grid approximation
let n = 50.
let probas = [0. .. (1. / n) .. 1.]
let flatPrior = [for _ in 0. .. n -> 1.]
let likelihood = [for p in probas -> (Discrete.binomial p 9).PDF 6]

(**
The target of inference in Bayesian inference is a posterior probability distribution.
*)

(**
Practice
*)

(**
Easy
*)

(**
2E1. Pr(rain|Monday)
2E2. The probability that it is Monday, given that it is raining.
2E3. Pr(Monday|rain); Pr(rain|Monday)Pr(Monday) / Pr(rain)
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

let multiGridApproximationFig priorFunc (points : list<int>) (nTrials : int) (nSucesses : int)= 
    points
    |> List.map (fun nPointsToEstimate -> 
        let probas = [0. .. (1. / (float nPointsToEstimate - 1.)) .. 
            if nTrials <> nSucesses then 1. 
            else 0.999999999999999]

        let prior, likelihood = 
            probas 
            |> List.map (fun p -> 
                priorFunc p, (Discrete.binomial p nTrials).PDF nSucesses)
            |> List.unzip
        
        Chart.Line(probas, computePosterior likelihood prior, Name = $"#Points: {probas.Length}"))
    |> Chart.combine
    |> Chart.withTitle("Grid approximate posterior distribution")
    |> Chart.withXAxisStyle("Proportion of water (parameter p)")
    |> Chart.withYAxisStyle("Posterior probability")
    |> Chart.withSize(650., 650.)

// (1) W,W,W
// N = 3; W = 3 ; L = 0
multiGridApproximationFig (fun x -> 1.) [10 .. 10 .. 100] 3 3

// (2) W,W,W,L
// N = 4; W = 3; L = 1
multiGridApproximationFig (fun x -> 1.) [10 .. 10 .. 100] 4 3

// (3) L,W,W,L,W,W,W
// N = 7; W = 5; L = 2
multiGridApproximationFig (fun x -> 1.) [10 .. 10 .. 100] 10 1

(**
2M2. Now assume a prior for p that is equal to zero when p < 0.5 and is a positive constant when
p ≥ 0.5. Again compute and plot the grid approximate posterior distribution for each of the sets of
observations in the problem just above.
*)

// (1) W,W,W
// N = 3; W = 3 ; L = 0
multiGridApproximationFig (fun x -> if float x < 0.5 then 0. else 1.) [10 .. 10 .. 100] 3 3 

// (2) W,W,W,L
// N = 4; W = 3; L = 1
multiGridApproximationFig (fun x -> if float x < 0.5 then 0. else 1.) [10 .. 10 .. 100] 4 3 

// (3) L,W,W,L,W,W,W
// N = 7; W = 5; L = 2
let a = multiGridApproximationFig (fun x -> if float x < 0.5 then 0. else 1.) [10 .. 10 .. 100] 7 5 

(**
testing
*)
a

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