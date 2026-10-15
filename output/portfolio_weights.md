# Global Minimum-Variance Portfolio Weights

The weights below are fixed portfolio weights estimated from the first 70% of
the common return sample. The optimisation is unconstrained, so negative weights
represent short positions.

| Ticker | Sample covariance | Linear shrinkage | Nonlinear shrinkage |
|---|---:|---:|---:|
| ABG.JO | 2.69% | 2.39% | 2.48% |
| AGL.JO | -0.55% | -0.32% | -0.36% |
| APN.JO | 6.34% | 6.59% | 6.32% |
| BVT.JO | 5.25% | 5.50% | 5.48% |
| CPI.JO | 4.00% | 4.00% | 3.89% |
| FSR.JO | -6.99% | -5.97% | -6.87% |
| GFI.JO | 8.47% | 8.60% | 8.46% |
| MNP.JO | 25.51% | 24.78% | 25.30% |
| MTN.JO | -1.73% | -1.38% | -1.60% |
| NED.JO | -1.98% | -1.81% | -2.01% |
| NPN.JO | 10.02% | 10.21% | 10.21% |
| REM.JO | 13.05% | 12.19% | 12.83% |
| SBK.JO | 0.31% | 0.24% | 0.15% |
| SHP.JO | 7.40% | 7.61% | 7.32% |
| SOL.JO | -0.82% | -0.76% | -0.82% |
| VOD.JO | 28.48% | 27.19% | 28.15% |
| WHL.JO | 0.55% | 0.95% | 1.07% |

## Concentration summary

| Measure | Sample covariance | Linear shrinkage | Nonlinear shrinkage |
|---|---:|---:|---:|
| Combined VOD and MNP weight | 53.98% | **51.97%** | 53.45% |
| Combined four largest weights | 77.06% | **74.36%** | 76.48% |
| Total absolute short exposure | 12.07% | **10.25%** | 11.65% |
| Gross leverage | 1.241 | **1.205** | 1.233 |

## Interpretation

All three minimum-variance portfolios are concentrated. Vodacom and Mondi alone
account for approximately 52% to 54% of each portfolio, while the four largest
positions account for approximately 74% to 77%. Linear shrinkage produces the
least concentrated result, but it does not eliminate concentration.

The sample-covariance portfolio has approximately 12.1% total short exposure.
Linear shrinkage reduces this to approximately 10.3%, principally by making the
short position in FirstRand less negative. Nonlinear shrinkage remains closer to
the sample portfolio.

## Suggested essay wording

> The estimated weights reinforce the stability results. Under the sample
> covariance matrix, Vodacom and Mondi represented approximately 54% of the
> portfolio, while total absolute short exposure was 12.1%. Linear shrinkage
> reduced their combined allocation to approximately 52% and short exposure to
> 10.3%. Shrinkage therefore moderated extreme positions, although substantial
> portfolio concentration remained.

These weights should be used as supporting evidence rather than reproduced in
full in the main essay. A short concentration summary is sufficient for the
empirical section.
