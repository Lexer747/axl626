# Preliminaries

I want to create a real world useful piece of software which i can actually use to determine
which stocks would be the best investment.

Ideally it has minimal inputs, point it towards a stock market, give it a portfolio and a risk
tolerance. From there work out the best way to allocate the portfolio that is predicted to 
produce the best return.

---

I.e.

Starting Portfolio: £50,000

Asset class | Year 1  | After 3 years | Re balance
------------|---------|---------------|----------
Cash        | £5,000  | £5,125        | £5,326
Stocks      | £45,000 | £48,135       | £47,934

The ratio between cash and stocks is maintained through out (1:9), even if it involves selling
some stocks.

In reality it is more likely that they're are many more asset classes used, and the ratio 
is based more the on the overall volatility of each asset. As well as the level of acceptable 
risk and expected return.

## Existing systems

Paying a financial advisor to do the labor for you is a perfectly valid strategy. Manually
reallocating the portfolio into different market areas. But it has the problem of the human
condition will stay static and unwilling to change. For example a stock has risen in value
by 5% over the 3 years. And the overall allocation of the portfolio has now been thrown out
of balance. A human may still keep the stock, but this could lead to a greater risk, and 
less great reward.

There does exist some websites which claim to do this role. One example is https://www.wealthfront.com
which when I faux used to test it. I had a questionnaire, which asked me about the following 
topics:

* Primary reason for investing?
    * General savings
    * Retirement
    * College savings
    * Other
* What are you looking for in a financial advisor?
    * I'd like a diversified investment portfolio
    * Save money on taxes
    * I'd like someone to complete manage my investments
    * I'd like to match or beat the performance of the markets
* Whats your age?
* Whats your income
* Describe your household
    * Single income, no dependents
    * Single income, 1 or more dependents
    * Dual income, no dependents
    * Dual income, 1 or more dependents
    * Retired
* Whats your total value of your liquid investments
* What do you value more?
    * Maximizing gains
    * Minimizing losses
    * Both equally
* If your entire portfolio lost 10% of its value in a month, what would you do?
    * Sell your investments
    * Sell some
    * Keep all
    * Buy more

Once this was completed this screen was presented:

> ![](images/wealthfront_allocation.png)

This kind of system would be something I would like to create. But instead of a focus
on the allocation across assets. I would create a smaller piece of the puzzle, my software
would be ideally focused on just stocks. It would be the software that receives
27% + 35% of the portfolio, and uses that value as its input to calculate the best 
stocks to invest in.

In practice it could be used once the user has split their own portfolio into asset classes.
They take their calculated stock investment, and put it into my program with their
desired market, and use that to invest their money. 



## Feature Analysis

These are the features which are most required to make a system similar to or what already
exists.

Features:

* Inputs 
    - Portfolio
    - Risk tolerance
        * can be extrapolated from a questionnaire
        * or given as an amount the person is willing to loose from their portfolio
    - Inflation rate over time (aka current climate)
    - One or markets
        * to begin with simply start with the dataset provided by my supervisor
        * Make it capable to add new markets which are preprocessed to provide evaluation
* Outputs
    - Expected returns as a graph from the current date
    - Variance, show worst case and best case scenarios
    - Navigate the analyzed stocks to see the determined risk and reward for each stock

## Identifying Complex problems

Finding the risk of a stock. Using correlation and variance to find the risk
of each stock.

Given a stock in the format of a series of trades. How do we compute
the risk of the stock. Simply put over the entire lifespan of the stock
its risk is simply its variance. A higher variance is a riskier stock.

But we want to find the risk of a stock, in a set of stocks, given the
variance of each one. And how correlated it is to the other stocks.

How do we go about defining this mathematically?

## Mathematical risk definition

Lets start with an example, here are 2 stocks, which have some correlation.

`N` | X<sub>1</sub> | Y<sub>1</sub> | p<sub>1</sub>
----|---|-------|-------
1   |50 |59     |9
2   |50 |68     |18
3   |50 |57     |7
4   |50 |51     |1
5   |50 |60     |10
6   |50 |45     |-5
7   |50 |47     |-3
8   |50 |33     |-17

---

`N` | X<sub>2</sub> | Y<sub>2</sub> | p<sub>2</sub>
----|---|-------|-------
1   |50 |61     |11
2   |50 |60     |10
3   |50 |57     |7
4   |50 |67     |17
5   |   |       |
6   |50 |70     |20
7   |50 |19     |-31
8   |   |       |

I have deliberately put some empty spots in the second stock, to more
accurately depict the real world. And here the `N` value represents a
date, i.e. the same time the trade occurred.

First we have too choose a window size, and a weight to give the correlation.
The window is so that data that is either too old or too futuristic
doesn't impact the calculation for a specific risk at a date.

P<sub>i</sub>(>0) = (1/σ<sub>i</sub>) *  φ (μ<sub>i</sub> / σ<sub>i</sub>)

R<sub>1, 2</sub>(`N`) = P<sub>1</sub> * P<sub>2</sub> * w

where:

* p<sub>i</sub> = the variable representing the ith stock
* W = window size
* w = weight applied to the correlation
* σ<sub>i</sub> = the standard deviation for p<sub>i</sub> based around the
window, = Var(`N` - (W / 2), p<sub>i</sub>, W)
* Var(x, y, z) = the variance function, takes a central point (x) of a
variable (y), and a window size (z)
* μ<sub>i</sub> = the mean of p<sub>i</sub> based around the window
* φ (x) = the normal distribution function

Var(x, y, z) = ((x + (z / 2)) Σ i=(x - (z / 2)) {(y<sub>i</sub> - **y<sub>z</sub>**)<sup>2</sup>}) / z

where:

* **y<sub>z</sub>** = the mean of the variable y given a window of z
* y<sub>i</sub> = the profit of the trade at ith value

---

Sample with:

* `N` = 4
* W = 6
* w = 0.5

---
We want to find:  
* R<sub>1, 2</sub>(4)

So we need to compute this intermediate values:  
* P<sub>1</sub> and P<sub>2</sub>

Which requires finding the variance and mean of both variables:  
Var(4, p<sub>1</sub>, 6)  
((4 + (6 / 2)) Σ i=(4 - (6 / 2) {(y<sub>i</sub> - **y<sub>6</sub>**)<sup>2</sup>}) / 6  

> **y<sub>6</sub>** = (9 + 18 + 7 + 1 + 10 + -5 + -3) / 7  
> **y<sub>6</sub>** = 5.286...

(7 Σ i=1 {(y<sub>i</sub> - 5.286)<sup>2</sup>}) / 6  

 i | y | =
---|---|---
1 |(9 - 5.286 )<sup>2</sup>|13.793796 
2 |(18 - 5.286)<sup>2</sup>|161.645796
3 |(7 - 5.286 )<sup>2</sup>|2.937796  
4 |(1 - 5.286 )<sup>2</sup>|18.369796 
5 |(10 - 5.286)<sup>2</sup>|22.221796 
6 |(-5 - 5.286)<sup>2</sup>|105.801796
7 |(-3 - 5.286)<sup>2</sup>|68.657796 
  | | | Σ = 393.428572

Var(4, p<sub>1</sub>, 6) = 393.428572 / 6 = 65.571...

Other variance:  
Var(4, p<sub>2</sub>, 6)  
((4 + (6 / 2)) Σ i=(4 - (6 / 2) {(y<sub>i</sub> - **y<sub>6</sub>**)<sup>2</sup>}) / 6  

> **y<sub>6</sub>** = (11 + 10 + 7 + 17 + *missing data* + 20 + -31) / 7  
> **y<sub>6</sub>** = (11 + 10 + 7 + 17 + 20 + -31) / 6  
> **y<sub>6</sub>** = 5.666...

(7 Σ i=1 {(y<sub>i</sub> - 5.666)<sup>2</sup>}) / 6  

i | y | =
---|---|---
1 |(11- 5.666 )<sup>2</sup>|28.451556
2 |(10 - 5.666)<sup>2</sup>|18.783556
3 |(7 - 5.666 )<sup>2</sup>|1.779556
4 |(17 - 5.666 )<sup>2</sup>|128.459556
5 |(*missing data* - 5.666)<sup>2</sup>| *missing data* 
6 |(20 - 5.666)<sup>2</sup>|205.463556
7 |(-31 - 5.666)<sup>2</sup>|1344.395556
  | | | Σ = 1727.333336
  
Var(4, p<sub>2</sub>, 6) = 1727.333336 / 5 = 345.466...

---

Back to:  
> P<sub>1</sub> = (1/σ<sub>1</sub>) *  φ (μ<sub>1</sub> / σ<sub>1</sub>)  
> P<sub>1</sub> = (1/√65.571) *  φ (5.286 / √65.571)  
> P<sub>1</sub> = (0.123) *  φ (0.653)  
> P<sub>1</sub>(>0) = 0.7431
>
> P<sub>2</sub> = (1/σ<sub>2</sub>) *  φ (μ<sub>2</sub> / σ<sub>2</sub>)  
> P<sub>2</sub> = (1/√345.466) *  φ (5.666 / √345.466)  
> P<sub>2</sub> = (0.0538) *  φ (0.305)  
> P<sub>2</sub>(>0) = 0.6198

And finally:
> R<sub>1, 2</sub>(`N`) = P<sub>1</sub> * P<sub>2</sub> * w  
> R<sub>1, 2</sub>(4) = 0.7431 * 0.6198 * 0.5  
> R<sub>1, 2</sub>(4) = 0.230

