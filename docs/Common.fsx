(**
---
title: Common
category: Scripts
categoryindex: 1
index: 1
---
*)

namespace Common

#r "nuget: Plotly.NET, 3.0.0"
#r "nuget: Plotly.NET.Interactive, 3.0.0"
#r "nuget: Numpy, 3.10.1.29"
#r "nuget: MathNet.Numerics, 5.0.0"

open Numpy
open Plotly.NET
open MathNet.Numerics
open MathNet.Numerics.Distributions

module GridApproximation = 
    let posteriorProbabilities prior likelihood : array<float> = 
        let unstandardizedPosterior = 
            prior
            |> Array.zip likelihood
            |> Array.map (fun (ll, p) -> ll * p)
        
        let sum = Array.sum unstandardizedPosterior
        
        unstandardizedPosterior 
        |> Array.map (fun xs -> xs / sum)

    let binomialPosterior prior nTrials nSuccesses = 
        Generate.LinearSpaced(Seq.length prior, 0., 1.)
        |> Array.map (fun p -> Binomial.PMF(p=p, n=nTrials, k=nSuccesses))
        |> posteriorProbabilities prior

    let multiBinominalPosteriorFig priorFunc points nTrials nSuccesses = 
        points
        |> Seq.map (fun xs -> 
            let probas = [|0. .. (1. / (float xs - 1.)) .. 1.|]
            let prior = probas|> Array.map priorFunc
            Chart.Line(probas, binomialPosterior prior nTrials nSuccesses, Name = $"# Grid Points: {probas.Length}"))
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