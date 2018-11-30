# Papers:

1. Kelly Criterion (file:///C:/Users/Lexer/Documents/Uni/FYP/kelly_56.pdf)

1. Kelly Criterion in Portfolio Optimizations (file:///C:/Users/Lexer/Documents/Uni/FYP/KellyCriterionPortfolioOptimization.pdf)

1. The Handbook of Portfolio Mathematics (Book ISBN: 978-0-471-75768-9)

1. Kelly Criterion: You Don't Know the half of it (https://blogs.cfainstitute.org/investor/2018/06/14/the-kelly-criterion-you-dont-know-the-half-of-it/)


## Kelly Criterion

Kelly criterion is a well known concept for the most optimal way to grow a budget from a given
gamble. In the case where there is a positive expectation gamble, then using the Kelly criterion
is mathematically proven to be optimal. Based on the paper which under the guise of networking
formally defines the optimal growth rate of a betting function with a fixed probability and
payout. [Kelly Criterion]

```
     bp - (1 - p)
k = --------------
          b
```
Where:
* `k` is the bet size as a fraction of your portfolio
* `b` is the net odds on the wager. I.e. you win `b` + the wager staked
* `p` is the probability of winning

---

This has one clear difference to real stocks and investment which is that you are assumed to
lose 100% of your wager upon a loss. Which while technically true in the worst case for a stock
or bond investment. Is more often than not, not what happens. Typically you lose a smaller
percentage than 100%. Hence there is an adapted formula: [Kelly Criterion: You Don't Know the half of it]

```
     p - (1 - p)
k =  -----------
        (W/L)
```

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

```
E(X) = 0.6 * 1.2 + 0.4 * 0.8 = 1.04
```

So the expected value of `X` is a return of 4%. And putting it into the kelly formula, it tells
us how much of portfolio we should bet to see optimal growth.

```
k = 0.6 - (1 - 0.6) / (0.2 / 0.2)
k = 0.2
```

Hence we should bet 20% of our portfolio on the next trade. The problem with this approach is
that its a ratio. So if our gamble is more volatile (or equally any stock) and a win is 
200% and a loss is 200%, then the ratio between wins and losses is the same. But even
though the investment is 10x more volatile, it still has the same kelly value:

```
k = 0.6 - (1 - 0.6) / (2 / 2)
k = 0.2
```

### The correct kelly formula

```
k = p/L - (1 - p)/W
```

As you can see this no longer has the ratio of wins and losses as the denominator

```
k = 0.6/0.2 - (1 - 0.6)/0.2
k = 1
```

As you can see, this Kelly value says to put 100% of your portfolio on the bet. Which might seem
a bit ridiculous but this is why people generally use a fraction of Kelly value, as even though
it is mathematically the fastest way to grow a portfolio from a given gamble. It can be daunting
to invest that much.

```
k = 0.6/2 - (1 - 0.6)/2
k = 0.1
```

But as we can see from the above calculation with a Win of 200% and a loss of 200%, it correctly
adjusts the bet size so that only 10% of the portfolio is invested. Whereas the other Kelly
formula would not to change the bet size.

## Kelly Criterion Decoupled Problem

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

```
Estimated Geometric Mean = sqrt(Arithmetic Mean ^ 2 - Population Standard Deviation ^ 2)

Arithmetic mean = Σ trades / total num of trades

Population Standard Deviation = (1/(N - 1)) * N Σ i=1 {(Xi - Arithmetic Mean)^2}
```

Where:
* `Xi` is a i'th trade
* `N` is the number of trades

