(**
---
title: Common
category: Scripts
categoryindex: 1
index: 1
---
*)

namespace Common

#r "nuget: FSharp.Stats, 0.4.6"
#r "nuget: Numpy, 3.10.1.29"
#r "nuget: Plotly.NET, 3.0.0"
#r "nuget: Plotly.NET.Interactive, 3.0.0"

open System
open Numpy
open Plotly.NET
open FSharp.Stats.Distributions

module rethinking = 
    let private r = System.Random(0)

    let computePosterior likelihood prior : array<float> = 
        prior
        |> Array.zip likelihood
        |> Array.map (fun (likelihood_i, prior_i) -> likelihood_i * prior_i)
        |> fun unstandardizedPosterior -> 
            let sum = Array.sum unstandardizedPosterior
            unstandardizedPosterior |> Array.map (fun xs -> xs / sum)
    
    let binomialMultiGridApproximationFig priorFunc (points : list<int>) (nTrials : int) (nSucesses : int)= 
        points
        |> List.map (fun nPointsToEstimate -> 
            let probas = [|0. .. (1. / (float nPointsToEstimate - 1.)) .. 
                if nTrials <> nSucesses then 1. 
                else 0.999999999999999|]

            let prior, likelihood = 
                probas 
                |> Array.map (fun p -> 
                    priorFunc p, (Discrete.binomial p nTrials).PDF nSucesses)
                |> Array.unzip
            
            Chart.Line(probas, computePosterior likelihood prior, Name = $"# Grid Points: {probas.Length}"))
        |> Chart.combine
        |> Chart.withTitle("Grid approximate posterior distribution")
        |> Chart.withXAxisStyle("Proportion of water (parameter p)", MinMax=(0., 1.))
        |> Chart.withYAxisStyle("Posterior probability")
        |> Chart.withSize(650., 650.)

module NumpyUtils = 
    let toNDarray (xs : seq<_>) : NDarray = 
        np.asarray<_>(Seq.toArray xs)

    let toSharp (xs : NDarray) : array<float> = 
        xs.astype(np.float_).GetData<float>()