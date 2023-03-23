For a range of S and N:

- Draw from a Fisher logseries
    - This can be accomplished either by using untb::fisher_ecosystem or using the meteR workflow. They are sort of theoretically equivalent, I think, but untb gives variability in S and N and meteR only gives variability in N. 
    - Update after talking to Andy: Fisher's alpha is equal to theta, and S comes from alpha as: alpha (ln(1 + J / alpha)). So at the very least you don't need to use `hubbell` to get to S from J and v. 
- Compare to draws from the feasible set
    - This can be accomplished either by comparing every draw to its FS (expensive) or by computing distributions of summary statistics across draws and comparing them (faster, but with wiggle room due to variation in S and N from logseries draws, and with the trick of comparing two distributions).
    
For these purposes I'm not *so* interested in a really robust test of statistical significance or effect size so much as "is the LS high/low and does this vary over the state space". The percentile scores alone would give you that - e.g. a distribution of the fisher draw %iles relative to the fs. That may tell you about what you get from a t/wilcoxon test. (Especially because the actual values, and importance of an effect size, vary over S and N...OH WAIT MB NOT FOR HILL NUMBERS, so maybe try the Wilcoxon?)


Steps ("for a given S and J"):

- Draw n draws from the fisher ls using untb (or sads?)
- Draw n draws from the feasible set
- Compute hill numbers for all draws
- Perform wilcox rank sum test comparing FS to fishing
- Final d.t. with columns for S, J, p value, median difference 