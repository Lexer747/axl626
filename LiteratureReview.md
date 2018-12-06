# Papers:

1. Kelly Criterion (file:///C:/Users/Lexer/Documents/Uni/FYP/kelly_56.pdf)

1. Kelly Criterion in Portfolio Optimizations (file:///C:/Users/Lexer/Documents/Uni/FYP/KellyCriterionPortfolioOptimization.pdf)

1. The Handbook of Portfolio Mathematics (Book ISBN: 978-0-471-75768-9, file:///C:/Users/Lexer/Documents/Uni/FYP/The_Hand_Book_Of_Portfolio_Mathematics.pdf)

1. Kelly Criterion: You Don't Know the half of it (https://blogs.cfainstitute.org/investor/2018/06/14/the-kelly-criterion-you-dont-know-the-half-of-it/)


## Kelly Criterion

Kelly criterion is a well known concept for the most optimal way to grow a budget from a given
gamble. In the case where there is a positive expectation gamble, then using the Kelly criterion
is mathematically proven to be optimal. Based on the paper which under the guise of networking
formally defines the optimal growth rate of a betting function with a fixed probability and
payout. [Kelly Criterion]


k = (bp - (1 - p)) / b

Where:
* `k` is the bet size as a fraction of your portfolio
* `b` is the net odds on the wager. I.e. you win `b` + the wager staked
* `p` is the probability of winning

---

This has one clear difference to real stocks and investment which is that you are assumed to
lose 100% of your wager upon a loss. Which while technically true in the worst case for a stock
or bond investment. Is more often than not, not what happens. Typically you lose a smaller
percentage than 100%. Hence there is an adapted formula: [Kelly Criterion: You Don't Know the half of it]


k = (p - (1 - p)) / (W/L)


Where:
* `W` is the amount of winnings
* `L` is the amount of losses

### Example 

Say we have a gamble `X` with the following properties:

P(`X`) = 0.6

And `X` pays out 1.2x the stake. (win 20%)

And in the case 1 - P(`X`) = 0.4

`X` only takes 0.8x of the stake, instead of all of it. (lose 20%)

---

As we can see `X` is a good investment:

> E(X) = 0.6 * 1.2 + 0.4 * 0.8 = 1.04

So the expected value of `X` is a return of 4%. And putting it into the kelly formula, it tells
us how much of portfolio we should bet to see optimal growth.


> k = 0.6 - (1 - 0.6) / (0.2 / 0.2)
> k = 0.2


Hence we should bet 20% of our portfolio on the next trade. The problem with this approach is
that its a ratio. So if our gamble is more volatile (or equally any stock) and a win is 
200% and a loss is 200%, then the ratio between wins and losses is the same. But even
though the investment is 10x more volatile, it still has the same kelly value:

> k = 0.6 - (1 - 0.6) / (2 / 2)
> k = 0.2

### The correct kelly formula


k = p/L - (1 - p)/W


As you can see this no longer has the ratio of wins and losses as the denominator


> k = 0.6/0.2 - (1 - 0.6)/0.2
> k = 1


As you can see, this Kelly value says to put 100% of your portfolio on the bet. Which might seem
a bit ridiculous but this is why people generally use a fraction of Kelly value, as even though
it is mathematically the fastest way to grow a portfolio from a given gamble. It can be daunting
to invest that much.

> k = 0.6/2 - (1 - 0.6)/2
> k = 0.1

But as we can see from the above calculation with a Win of 200% and a loss of 200%, it correctly
adjusts the bet size so that only 10% of the portfolio is invested. Whereas the other Kelly
formula would not to change the bet size.



## Kelly Criterion Application

## Optimal F

As described by [The Handbook of Portfolio Mathematics]:

> For any given independent trials situation where you have an edge (i.e. positive expectation,
> see Kelly Criterion for an example) there exists an optimal fixed fraction (f) between 0 and
> 1 as a divisor of your biggest loss to bet on each event.

As it turns out this optimal f is similar to what Kelly describes in his paper. And as
Ralph proceeds, he explains how Kelly criterion is a perfect solution for fixed size
gambles with fixed wins and losses:

```
f = Mathematical Expectation/(W/L)
```

But then the book reaches the a different conclusion, it goes on to state that trades where the
win or loss is always changing (like the stock market) then Kelly formula does find the correct
optimal f. This lends to reason as it is a dream like world where we have a fixed unchanging
trade with a set win loss ratio.

So instead the book proposes finding the optimal f by instead using the geometric mean.
We can use the estimated geometric mean because it is basically the same, while being much
less computation:

TWR = N Π i=1 { (1 + f * (-trade<sub>i</sub> / biggest loss) }

Geometric Mean = exp((1/N) * ln(TWR))

And to maximize our profit from a set of trades we want to optimize for the highest possible
geometric mean. Since f is a free standing variable which cannot be made the subject of the
equation we can only use iteration to find a good estimate.

## Example

Given we trade `N` stocks buy buying them at point `X` and selling them at point `Y` we have
a sample trade sequence:

`N` |`X`| `Y`   | profit
----|---|-------|-------
1   |50 |59     |9
2   |50 |68     |18
3   |50 |57     |7
4   |50 |51     |1
5   |50 |60     |10
6   |50 |45     |-5
7   |50 |47     |-3
8   |50 |33     |-17

Note how the loss profit is different on each trade, hence Kelly's fraction to bet would be
incorrect. And starting with an arbitrary f of 0.1.

`N` | i                 | TWR
----|-------------------|---
1   |1 + 0.1 * (-9/-17) | 1.0529
2   |1 + 0.1 * (-18/-17)| 1.1059
3   |1 + 0.1 * (-7/-17) | 1.0412
4   |1 + 0.1 * (-1/-17) | 1.0059
5   |1 + 0.1 * (-10/-17)| 1.0588
6   |1 + 0.1 * (5/-17)  | 0.9706
7   |1 + 0.1 * (3/-17)  | 0.9824
8   |1 + 0.1 * (17/-17) | 0.9000
--  |                   | = 1.1081

Geometric mean = exp((1/8) ln(1.1081)) = 1.0129

> Note: Since the geometric mean is exponentially dependent on the TWR, we can actually skip
> the step to calculate the geometric mean.

But this does not tell us the optimal fraction to bet, we simply chose a random fraction 0.1,
so we should now calculate the mean with a different f; say 0.9.

`N` | i                 | TWR
----|-------------------|---
1   |1 + 0.9 * (-9/-17) | 1.4764
2   |1 + 0.9 * (-18/-17)| 1.9529
3   |1 + 0.9 * (-7/-17) | 1.3705
4   |1 + 0.9 * (-1/-17) | 1.0529
5   |1 + 0.9 * (-10/-17)| 1.5294
6   |1 + 0.9 * (5/-17)  | 0.7352
7   |1 + 0.9 * (3/-17)  | 0.8411
8   |1 + 0.9 * (17/-17) | 0.0099
--  |                   | = 0.3936

As we can see the TWR is much lower so we know that it is a worse f to use for betting. Hence
we know a better f exists inside the 2 bounds. Solving for f by iteration:

Optimal f: 0.2370

TWR = 1.0957

[The Handbook of Portfolio Mathematics]

## Portfolio Diversification

Finding the optimal f, or the fraction to bet is only a small part of actually determining how
to spread out a bank roll over multiple markets. At the end of day portfolio allocation is about
the mathematics of a system, rather than a single trade or bet. Consider a 2:1 coin game
which has favorable mathematically expectation, the optimal f is 0.25.

But consider simultaneously playing 2 games, (for simplicity another 2:1 coin game) the optimal
f becomes a function of correlation between both games. As if these games are not independent
events, much like stocks on the market are not. Then the outcome of the first game affecting
the second game, or vice versa. Will affect the optimal f.

Thinking of the optimal f as a single point, inside a space of trades/bets. When we have
only a single trade/bet, we are doing calculations in a 2D space, and we have a line which
represents out optimal f. However as the number of trades/bets increases we gain another
dimension for each one.

Therefore instead a f which lies on a line, it lies on a plane, or a 3D object, and etc.
If the f suddenly becomes a multi variable coordinate which must be exactly correct.
If a single axis is out then you can miss the hill of positive growth, even if every other
axis lined up.

This means we need to define a new function to find the individual f's for all the bets, but
with relation to each other.

#### Mathematical Definition

---

G(f<sub>*1*</sub>...f<sub>*n*</sub>) = (*m* Π *k*=1 {HPR<sub>*k*</sub>})<sup>(1 / *m* Σ *k*=1 {Prob<sub>*k*</sub>})

where:  
* *n* = The number of trades/bets
* *m* = The number of combinations for all the trades and bets, i.e. for each *n* which has *x*
    outcomes, then its: *n* Π *i*=1 {*x*<sub>*i*</sub>} (*x* is normally 2, win and lose)

HPR<sub>*k*</sub> = (1 + (*n* Σ *i*=1 {f<sub>*i*</sub> * (-PL<sub>*k,i*</sub> / BL<sub>*i*</sub>) }) )<sup>Prob<sub>*k*</sub></sup>

where:
* f<sub>*i*</sub> = The optimal f for that *i*, where f > 0
* PL<sub>*k,i*</sub> = The outcome profit or loss for the *i*th trade/bet associated with the
    *k*th outcome
* BL<sub>*i*</sub> = The worst outcome of trade/bet

Prob<sub>*k*</sub> = (*n* - 1 Π *i*=1 {*n* Π *j*=*i*+1 { P(*i<sub>k</sub>* | *j<sub>k</sub>*) }}) <sup> (1 / (*n* - 1)) </sup>

This can be combined into one larger equation:

G(f<sub>*1*</sub>...f<sub>*n*</sub>) = (*m* Π *k*=1 {(1 + *n* Σ *i*=1 {f<sub>*i*</sub> * (-PL<sub>*k,i*</sub> / BL<sub>*i*</sub>)})<sup>Prob<sub>*k*</sub></sup>})<sup>(1 / *m* Σ *k*=1 {Prob<sub>k</sub>})

[The Handbook of Portfolio Mathematics]

### Sample from this model

Applying G to a setting in which we actually want to optimize how we bet our portfolio. 
Say we have 3 coins, Coin 1, Coin 2, Coin 3. Each one has 2 outcomes, and for simplicity we
assume that the correlation coefficients are zero. I.e. each coin is independent.

This means we need to find 3 f values (f<sub>1</sub>, f<sub>2</sub>, f<sub>3</sub>) to result
in the greatest growth. Or the 3 f's which result in the greatest G.

Re-writing HPR<sub>k</sub> to:

HPR<sub>*k*</sub> = (1 + C)<sup>Prob<sub>*k*</sub></sup>

C =  *n* Σ *i*=1 {f<sub>*i*</sub> * (-PL<sub>*k,i*</sub> / BL<sub>*i*</sub>) }

We can calculate C. Given that BL is the same for each coin, say -1, and the PL is -1. I.e.
when we lose we gain -£1, when we loss our biggest loss is -£1. This is because *k*=1 so this
the first outcome, where it lands tails. If we were to win then PL would be 2, as we are playing
a 2:1 game.

Once again we have to use f<sub>*i*</sub> without knowing what it is, so pick a random start of
0.1. And *n* is 3 since we have 3 coins.

> (0.1 * (- -1 / -1)) + (0.1 * (- -1 / -1)) + (0.1 * (- -1 / -1))  
> (0.1 * -1) + (0.1 * -1) + (0.1 * -1)  
> -0.3

> HPR<sub>*k*</sub> = (1 + -0.3)<sup>Prob<sub>*k*</sub></sup>  
> HPR<sub>*k*</sub> = (0.7)<sup>Prob<sub>*k*</sub>

Now we need to calculate:

Prob<sub>*k*</sub> = (*n* - 1 Π *i*=1 {*n* Π *j*=*i*+1 { P(*i<sub>k</sub>* | *j<sub>k</sub>*) }}) <sup> (1 / (*n* - 1)) </sup>

The important part of this is P(*i<sub>k</sub>* | *j<sub>k</sub>*), as this the joint 
probability of the scenario *i* having the outcome *k*. I.e:
* *i* = 1, *k* = 1 -> Coin 1, tails -> 0.5
* *i* = 1, *k* = 2 -> Coin 1, heads -> 0.5
* *i* = 2, *k* = 1 -> Coin 2, tails -> 0.5
* etc...

Joint with the probability of scenario *j* having the outcome *k*. I.e:
* *j* = 1, *k* = 1 -> Coin 1, tails -> 0.5
* *j* = 1, *k* = 2 -> Coin 1, heads -> 0.5
* *j* = 2, *k* = 1 -> Coin 2, tails -> 0.5
* etc...

So P(*i<sub>k</sub>* | *j<sub>k</sub>*), where *n* = 3:

*i* | *j* | *k* | P(*i<sub>k</sub>* \| *j<sub>k</sub>*)
----|-----|-----|-------------------
1   |2    |1    | P(0.5 ∩ 0.5) = 0.25
1   |3    |1    | P(0.5 ∩ 0.5) = 0.25
2   |3    |1    | P(0.5 ∩ 0.5) = 0.25

Since the events are independent we use the intersection of both events.

> Prob<sub>*k*</sub>  = (0.25 * 0.25 * 0.25) <sup> (1 / (3 - 1)) </sup>  
> Prob<sub>*k*</sub>  = (0.015625) <sup> (1 / (3 - 1)) </sup>  
> Prob<sub>*k*</sub>  = (0.015625) <sup> (1 / 2) </sup>  
> Prob<sub>*k*</sub>  = 0.125

Now we can calculate HPR<sub>*k*</sub> where *k* = 1:

> HPR<sub>*k*</sub> = (1 +C)<sup>Prob<sub>*k*</sub></sup>  
> HPR<sub>*k*</sub> = (0.7)<sup>Prob<sub>*k*</sub></sup>  
> HPR<sub>*k*</sub> = (0.7)<sup>0.125</sup>  
> HPR<sub>*k*</sub> = 0.95639490757  

To find G we need to find the HPR for all values of *k* from 1 to *m*:

finding *m*, since *n* is 3, and has 2 outcomes each:

> *m* = *n* Π *i*=1 {*x*<sub>*i*</sub>}  
> *m* = 2 * 2 * 2  
> *m* = 8  

And therefore *k* is an exhaustive list of outcomes:

*k* | Coin 1 | Coin 2 | Coin 3
----|--------|--------|---
1   |t       |t       |t
2   |t       |t       |h
3   |t       |h       |t
4   |t       |h       |h
5   |h       |t       |t
6   |h       |t       |h
7   |h       |h       |t
8   |h       |h       |h

Full table for G:

*k* | C  | HPR<sub>*k*</sub> | Prob<sub>*k*</sub>
----|----|-------------------|-------------------
1   |-0.3|0.9564             |0.125
2   |0   |1                  |0.125
3   |0   |1                  |0.125
4   |0.3 |1.0333             |0.125
5   |0   |1                  |0.125
6   |0.3 |1.0333             |0.125
7   |0.3 |1.0333             |0.125
8   |0.6 |1.0605             |0.125

Hence finding G:

> G(0.1, 0.1, 0.1) = (*m* Π *k*=1 {HPR<sub>*k*</sub>}) <sup>(1 / *m* Σ *k*=1 {Prob<sub>*k*</sub>}) </sup>  
> G(0.1, 0.1, 0.1) = (0.9564 * 1 * 1 * 1.0333 * 1 * 1.0333 * 1.0333 * 1.0605) <sup>(1 / *m* Σ *k*=1 {Prob<sub>*k*</sub>}) </sup>  
> G(0.1, 0.1, 0.1) = (1.1190) <sup>(1 / *m* Σ *k*=1 {Prob<sub>*k*</sub>}) </sup>  
> G(0.1, 0.1, 0.1) = (1.1190) <sup>(1 / (0.125 + 0.125 + 0.125 + 0.125 + 0.125 + 0.125 + 0.125 + 0.125))</sup>  
> G(0.1, 0.1, 0.1) = (1.1190) <sup>(1 / 1)</sup>  
> G(0.1, 0.1, 0.1) = 1.1190

Now to find the optimal f we can simply choose iterate over f<sub>*i*</sub> until we reach the
largest G.

And if you've been anywhere near AI for past few years, optimizing f's for the largest G is 
a classical problem which is often solved by neural networks or genetic algorithms. Its
gradient descent. Given an array of inputs, and a evaluation function, find the best array
of inputs to score the best in the evaluation function. In our case, the array is f, and
our function is G.

## Portfolio Optimization

This model of G is just one method, another model is suggested by [Kelly Criterion in Portfolio Optimizations]
in which the author combines the same model proposed above, with the addition of a risk model
in which each trade/bet when combined forms a linear correlation using an estimation technique
which can then be used to find the variance of the system.

I shall leave the specific implementation of this model to the reader if they wish to 
understand the latest and greatest models currently in use. This model also uses genetic 
algorithms to solve the model for the set of f. Since once again its a evaluation function, 
with a large set of inputs which can adjusted to find the optimum set of inputs which best
performs according to the function.

