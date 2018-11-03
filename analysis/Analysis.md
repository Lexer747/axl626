# Preliminaries

I want to create a real world useful piece of software which i can actually use to determine
which stocks would be the best investment.

Ideally it has minimal inputs, point it towards a stock market, give it a portfolio and a risk
tolerance. From there work out the best way to allocate the portfolio that is predicted to 
produce the best return.

I.e.

Starting Portfolio: £50,000

        | Year 1  | After 3 years | Re balance
--------|---------|---------------|----------
Cash    | £5,000  | £5,125        | £5,326
Stocks  | £45,000 | £48,135       | £47,934

---

Features:

* Inputs 
    - Portfolio
    - Risk tolerance
        * can be extrapolated from questionnaire
        * or given as an amount the person is willing to loose from their portfolio
    - Inflation rate over time
    - One or markets
        * to begin with simply start with the dataset provided by my supervisor
        * Make it capable to add new markets which are preprocessed to provide evaluation
* Outputs
    - Expected returns as a graph from the current date
        * how much of the portfolio should remain as liquid
        * maintaining the ratio of cash to stock, so that after the increase or decrease over the years, stocks are sold or bought to maintain the ratio
    - Variance, show worst case and best case scenarios
    - Navigate the analyzed stocks to see the determined risk and reward for each stock
    

## Existing systems.

