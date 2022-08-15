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
### NuGet packages
*)

#r "nuget: Plotly.NET, 3.0.0"
#r "nuget: Plotly.NET.Interactive, 3.0.2"
#r "nuget: MathNet.Numerics, 5.0.0"

open Plotly.NET
open Plotly.NET.Interactive
open MathNet.Numerics
open MathNet.Numerics.Distributions

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
**2E1.** Which of the expressions below correspond to the statement: *the probability of rain on Monday* ?

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
### **2M1**. Recall the globe tossing model from the chapter. Compute and plot the grid approximate posterior distribution for each of the following sets of observations. 
### In each case, assume a uniform prior for p.

For the globe tossing model, the random variable $X$ represents the number of times you observe *water* when tossing the globe.
 The true proportion of water covering the globe is $p$, and so the probability of observing *water* (W) on each single independent 
 toss is also $p$. Conversely, the probability or true proportion of *land* (L) covering the globe is $q = (1 - p)$.

The probability of getting exactly $k$ successes in $n$ independent Bernoulli trials is given by the probability mass function (PMF):

$$f(k,n,p)=Pr(k;n,p)=Pr(X=k)={n \choose x}p^{k}q^{n-k} \text{ , for } k = 0, 1, 2,  \ldots, n$$

$$n \in \mathbb{N} \text{ , } p \in [0,1]$$

Here, ${n \choose x} = \frac{n!}{k!(n-k)!}$, represents the number of ways "$k$ successes" can happen out of "$n$ trials."

For example, in the globe tossing model, if we consider $n=3$ and $k=1$:

Possible outcomes: 

$\{ LLL, \boldsymbol{LLW}, \boldsymbol{LWL} , \boldsymbol{WLL}, LWW , WWL, WWW \}$

Equivalently, 

${n \choose x} = \frac{3!}{( 1! (3 - 1)! )} = 3$

For each possible path, we have to account for the respective probabities $p$ and $q$. 
 Since each Bernoulli trial is independent we can simply multiply the probabilities $p$ and $q$ to obtain the probability of a successful path ($X=k$).
 In our example, $p \cdot p \cdot q$ or simply $p^{2} \cdot q$. And so $P(X=1)$ is the sum of the probabilities of all paths with exactly $1$ successes,
 which is ${3 \choose 1}p^{1}q^{2}$.
*)

let binomialBreakdownFig priorFunc nTrials nSucesses = 
    let title = sprintf  @"""$ \text{ Binomial distribution - } (n=%i,\: k=%i) $""" nTrials nSucesses
    let paramGrid = Generate.LinearSpaced(10000, 0., 1.)
    let prior = paramGrid |> Array.map priorFunc
    let likelihood = paramGrid |> Array.map (fun xs -> Binomial.PMF(p=xs, n=nTrials, k=nSucesses))

    [
        Chart.Line(paramGrid, prior, LineDash=StyleParam.DrawingStyle.Dot, Name="Prior")
        |> Chart.withYAxisStyle("Probability")
        
        Chart.Line(paramGrid, likelihood, LineDash=StyleParam.DrawingStyle.Dot, Name="Likelihood")
        |> Chart.withYAxisStyle("Probability")

        Chart.Line(paramGrid, posteriorProbabilities prior likelihood, Name="Posterior")
        |> Chart.withYAxisStyle("Probability")

    ]
    |> Chart.SingleStack(Pattern= StyleParam.LayoutGridPattern.Coupled)
    |> Chart.withLayoutGridStyle(YGap= 0.1)
    |> Chart.withXAxisStyle("""$ \text{ Parameter } p$""")
    |> Chart.withTitle(title)
    |> Chart.withSize(1000, 600)
    |> Chart.withMathTex(true)

(**
### 2M1. Uniform Prior | N=3, W=3, L=0
*)
(***do-not-eval***)
binomialBreakdownFig (fun x -> 1.) 3 3 |> Chart.show
(***hide***)
binomialBreakdownFig (fun x -> 1.) 3 3
|> GenericChart.toChartHTML
(*** include-it-raw ***)

(**
### 2M1. Uniform Prior | N=4, W=3, L=1
*)
(***do-not-eval***)
binomialBreakdownFig (fun x -> 1.) 4 3 |> Chart.show
(***hide***)
binomialBreakdownFig (fun x -> 1.) 4 3
|> GenericChart.toChartHTML
(*** include-it-raw ***)

(**
### 2M1. Uniform Prior | N=7, W=5, L=2
*)
(***do-not-eval***)
binomialBreakdownFig (fun x -> 1.) 7 5 |> Chart.show
(***hide***)
binomialBreakdownFig (fun x -> 1.) 7 5
|> GenericChart.toChartHTML
(*** include-it-raw ***)

(**
### **2M2**. Now assume a prior for $p$ that is equal to zero when $p < 0.5$ and is a positive constant when p > 0.5 . Again compute and plot the grid approximate posterior distribution for each of the sets of observations in the problem just above.
*)

(**
### 2M2. Heavyside step function Prior | N=3, W=3, L=0
*)
(***do-not-eval***)
binomialBreakdownFig (fun x -> if x < 0.5 then 0. else 1.) 3 3
(***hide***)
binomialBreakdownFig (fun x -> if x < 0.5 then 0. else 1.) 3 3
|> GenericChart.toChartHTML
(*** include-it-raw ***)

(**
### 2M2. Heavyside step function Posterior | N=4, W=3, L=1
*)
(***do-not-eval***)
binomialBreakdownFig (fun x -> if x < 0.5 then 0. else 1.) 4 3
(***hide***)
binomialBreakdownFig (fun x -> if x < 0.5 then 0. else 1.) 4 3
|> GenericChart.toChartHTML
(*** include-it-raw ***)

(**
### 2M2. (W,W,W) - Heavyside step function Prior | N=7, W=5, L=2
*)
(***do-not-eval***)
binomialBreakdownFig (fun x -> if x < 0.5 then 0. else 1.) 7 5
(***hide***)
binomialBreakdownFig (fun x -> if x < 0.5 then 0. else 1.) 7 5
|> GenericChart.toChartHTML
(*** include-it-raw ***)

(**
Computing the posterior distribution by grid approximation comes with its caveats. 
As shown below, the approximation will get closer to the analytically solved version of the posterior distribution.
*)

let multiBinominalPosteriorFig priorFunc points nTrials nSuccesses = 
    let title = sprintf @"""$ \text{Grid approximation - Posterior Distribution } (n=%i, k=%i)$" nTrials nSuccesses
    points
    |> Seq.map (fun xs -> 
        let probas = Generate.LinearSpaced(xs, 0., 1.)
        let prior = probas|> Array.map priorFunc
        Chart.Line(probas, binomialPosterior prior nTrials nSuccesses, Name = $"# Grid Points: {probas.Length}"))
    |> Chart.combine
    |> Chart.withTitle(title)
    |> Chart.withXAxisStyle(@"$ \text{Parameter } p$")
    |> Chart.withYAxisStyle("Posterior probability")
    |> Chart.withSize(1000, 500)

(***do-not-eval***)
multiBinominalPosteriorFig (fun x -> 1.) [10 .. 10 .. 100] 10 1
(***hide***)
multiBinominalPosteriorFig (fun x -> 1.) [10 .. 10 .. 100] 10 1
|> GenericChart.toChartHTML
(*** include-it-raw ***)

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

(**
Additional sources - 

1. https://en.wikipedia.org/wiki/Binomial_distribution
2. https://www3.nd.edu/~dgalvin1/10120/10120_S16/Topic19_8p6_Galvin.pdf
*)