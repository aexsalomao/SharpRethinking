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
### **2M1**. 
### Recall the globe tossing model from the chapter. Compute and plot the grid approximate posterior distribution for each of the following sets of observations. In each case, assume a uniform prior for p.

For the globe tossing model, let the random variable $X$ represent the number of times you observe *water* when tossing the globe.
 It is given that the true proportion of water covering the globe is $p$, and so the probability of observing *water* (W) on each single independent 
 toss is also $p$. Conversely, the probability or true proportion of *land* (L) covering the globe is $q = (1 - p)$.

The probability of getting exactly $k$ successes in $n$ independent Bernoulli trials is given by the probability mass function (PMF):

$$f(k,n,p)=Pr(k;n,p)=Pr(X=k)={n \choose k}p^{k}q^{n-k} \text{ , for } k = 0, 1, 2,  \ldots, n$$

$$n \in \mathbb{N} \text{ , } p \in [0,1]$$

Here, ${n \choose k} = \frac{n!}{k!(n-k)!}$, represents the number of ways "$k$ successes" can happen out of "$n$ trials."

For example, in the globe tossing model, if we consider $n=3$ and $k=1$:

Possible outcomes or paths: 

$\{ LLL, \boldsymbol{LLW}, \boldsymbol{LWL} , \boldsymbol{WLL}, WWL , WLW, LWW, WWW \}$

Equivalently, 

$\binom{n}{k} = \frac{3!}{1! \times (3 - 1)!} = 3$

For each possible path, we have to account for the respective probabilities $p$ and $q$. 
 Since each Bernoulli trial is independent we can simply multiply the probabilities $p$ and $q$ to obtain the probability of a successful path ($X=k$).
 In our example, the probability of observing every such path is $p \cdot p \cdot q$ or simply $p^{2} \cdot q$. 
 And so $P(X=1)$ is the sum of the probabilities of all paths with exactly $1$ successes:
 
$P(\boldsymbol{LLW})=P(\boldsymbol{LWL})=P(\boldsymbol{WLL})=p^{1}q^{2}$

$P(X=1)=P(\boldsymbol{LLW}) + P(\boldsymbol{LWL}) + P(\boldsymbol{WLL})=3\times p^{1}q^{2}$

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
### **2M1.** 
### Uniform Prior | N=3, W=3, L=0
*)
(***do-not-eval***)
binomialBreakdownFig (fun x -> 1.) 3 3 |> Chart.show
(***hide***)
binomialBreakdownFig (fun x -> 1.) 3 3
|> GenericChart.toChartHTML
(*** include-it-raw ***)

(**
### **2M1.** 
### Uniform Prior | N=4, W=3, L=1
*)
(***do-not-eval***)
binomialBreakdownFig (fun x -> 1.) 4 3 |> Chart.show
(***hide***)
binomialBreakdownFig (fun x -> 1.) 4 3
|> GenericChart.toChartHTML
(*** include-it-raw ***)

(**
### **2M1.** 
### Uniform Prior | N=7, W=5, L=2
*)
(***do-not-eval***)
binomialBreakdownFig (fun x -> 1.) 7 5 |> Chart.show
(***hide***)
binomialBreakdownFig (fun x -> 1.) 7 5
|> GenericChart.toChartHTML
(*** include-it-raw ***)

(**
### **2M2**. 
### Now assume a prior for $p$ that is equal to zero when $p < 0.5$ and is a positive constant when p > 0.5 . Again compute and plot the grid approximate posterior distribution for each of the sets of observations in the problem just above.
*)

(**
### **2M2.** 
### Heavyside step function Prior | N=3, W=3, L=0
*)
(***do-not-eval***)
binomialBreakdownFig (fun x -> if x < 0.5 then 0. else 1.) 3 3
(***hide***)
binomialBreakdownFig (fun x -> if x < 0.5 then 0. else 1.) 3 3
|> GenericChart.toChartHTML
(*** include-it-raw ***)

(**
### **2M2.** 
### Heavyside step function Posterior | N=4, W=3, L=1
*)
(***do-not-eval***)
binomialBreakdownFig (fun x -> if x < 0.5 then 0. else 1.) 4 3
(***hide***)
binomialBreakdownFig (fun x -> if x < 0.5 then 0. else 1.) 4 3
|> GenericChart.toChartHTML
(*** include-it-raw ***)

(**
### **2M2.** 
### Heavyside step function Prior | N=7, W=5, L=2
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
### **2M3.** 
### Suppose there are two globes, one for Earth and one for Mars. The Earth globe is 70% covered in water. The Mars globe is 100% land. Further suppose that one of these globes—you don’t know which—was tossed in the air and produced a “land” observation. Assume that each globe was equally likely to be tossed. Show that the posterior probability that the globe was the Earth, conditional on seeing “land” $Pr(Earth|Land) = 0.23$.
*)

(**

$\Pr(Earth \mid Land) = \frac{\Pr(Land \mid Earth) \times \Pr(Earth)}{\Pr(Land)} = \frac{\Pr(Land \mid Earth) \times \Pr(Earth)}{(\Pr(Earth) \times \Pr(Land \mid Earth)) + (\Pr(Mars) \times \Pr(Land \mid Mars))}$

*)

let formatNumb (x: float) = Math.Round(x, 2)

let prLandGivenEarth = 0.3
let prLandGivenMars = 1.
let prEarth = 0.5
let prMars = 0.5
let prLand = ((prMars * prLandGivenMars) + (prEarth * prLandGivenEarth))

(prLandGivenEarth * prEarth) / prLand
|> formatNumb
(*** include-it-raw ***)

(**
### **2M4.** 
### Suppose you have a deck with only three cards. Each card has two sides, and each side is either black or white. One card has two black sides. The second card has one black and one white side. The third card has two white sides. Now suppose all three cards are placed in a bag and shuffled. Someone reaches into the bag and pulls out a card and places it flat on a table. A black side is shown facing up, but you don’t know the color of the side facing down. Show that the probability that the other side is also black is 2/3. Use the counting method (Section 2 of the chapter) to approach this problem. This means counting up the ways that each card could produce the observed data (a black side facing up on the table).

3 Cards, 6 Conjectures:

$\blacksquare: (S_{1}=B, S_{2}=B), (S_{1}=B, S_{2}=B)$

$\bullet: (S_{1}=B, S_{2}=W), (S_{1}=W, S_{2}=B)$

$\blacktriangle: (S_{1}=W, S_{2}=W) (S_{1}=W, S_{2}=W)$

 # Ways B can appear as the side facing up: 3

 # Ways B can appear as the side facing down: 2

$\Pr(\blacksquare \mid S_{1}=B) = \frac{2}{3}$

Using Bayes theorem:

$\Pr(\blacksquare \mid S_{1}=B) = \frac{\Pr(S_{1}=B \mid \blacksquare) \times \Pr(\blacksquare)}{\Pr(S_{1}=B)} = \frac{1 \times \frac{1}{3}}{\frac{1}{2}} = \frac{2}{3}$
*)

(**
### **2M5.**
### Now suppose there are four cards: B/B, B/W, W/W, and another B/B. Again suppose a card is drawn from the bag and a black side appears face up. Again calculate the probability that the other side is black.

4th card: $\blacksquare$

 # Ways B can appear as the side facing up: 3 + 2 = 5

 # Ways B can appear as the side facing down: 2 + 2 = 4

$\Pr(\blacksquare \mid S_{1}=B) = \frac{4}{5}$

$\Pr(\blacksquare \mid S_{1}=B) = \frac{\Pr(S_{1}=B \mid \blacksquare) \times \Pr(\blacksquare)}{\Pr(S_{1}=B)} = \frac{1 \times \frac{2}{4}}{\frac{5}{8}} = \frac{2}{3}$

*)

(**
### **2M6.**
### Imagine that black ink is heavy, and so cards with black sides are heavier than cards with white sides. As a result, it’s less likely that a card with black sides is pulled from the bag. So again assume there are three cards: B/B, B/W, and W/W. After experimenting a number of times, you conclude that for every way to pull the B/B card from the bag, there are 2 ways to pull the B/W card and 3 ways to pull the W/W card. Again suppose that a card is pulled and a black side appears face up. Show that the probability the other side is black is now 0.5. Use the counting method, as before.

 # Ways B can appear as the side facing up: 1 ($\bullet$) + 2 ($\square$) -> ***2*** ($\bullet$) + 2 ($\square$) = 4

 # Ways B can appear as the side facing down: 2 ($\square$) = 2

$\Pr(\blacksquare \mid S_{1}=B) = \frac{2}{4} = \frac{1}{2}$

*)

(**
Additional sources - 

1. https://en.wikipedia.org/wiki/Binomial_distribution
2. https://www3.nd.edu/~dgalvin1/10120/10120_S16/Topic19_8p6_Galvin.pdf
*)