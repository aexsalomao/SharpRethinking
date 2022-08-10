(**
---
title: Chapter 2
category: Practice
categoryindex: 1
index: 2
---
*)


#load "Common.fsx"
#r "nuget: Plotly.NET, 3.0.0"
#r "nuget: Plotly.NET.Interactive, 3.0.2"
#r "nuget: Numpy, 3.10.1.29"
#r "nuget: MathNet.Numerics, 5.0.0"

open System
open System.IO

open Numpy
open Plotly.NET
open Plotly.NET.Interactive
open MathNet.Numerics
open MathNet.Numerics.Distributions

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
open Common
open Common.GridApproximation

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

// (1) W,W,W
// N = 3; W = 3 ; L = 0 
multiBinominalPosteriorFig (fun x -> 1.) [10 .. 10 .. 100] 3 3 |> GenericChart.toChartHTML
(*** include-it-raw ***)
(**
OK
*)

// (2) W,W,W,L
// N = 4; W = 3; L = 1
multiBinominalPosteriorFig (fun x -> 1.) [10 .. 10 .. 100] 4 3 |> Chart.show

// (3) L,W,W,L,W,W,W
// N = 7; W = 5; L = 2
multiBinominalPosteriorFig (fun x -> 1.) [10 .. 10 .. 100] 10 1 |> Chart.show

(**
2M2. Now assume a prior for p that is equal to zero when p < 0.5 and is a positive constant when
p ≥ 0.5. Again compute and plot the grid approximate posterior distribution for each of the sets of
observations in the problem just above.
*)

// (1) W,W,W
// N = 3; W = 3 ; L = 0
multiBinominalPosteriorFig (fun x -> if x < 0.5 then 0. else 1.) [10 .. 10 .. 100] 3 3 |> Chart.show

// (2) W,W,W,L
// N = 4; W = 3; L = 1
multiBinominalPosteriorFig (fun x -> if x < 0.5 then 0. else 1.) [10 .. 10 .. 100] 4 3 |> Chart.show

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