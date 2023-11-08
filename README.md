
# Feature Screening for High-dimensional Data in Classification Problems

## Introduction
In high-dimensional classification challenges, the presence of numerous irrelevant variables can impair the effectiveness of classifiers. Identifying and discarding these variables is crucial for improved accuracy, making feature screening a pivotal aspect of research in classification problems.

## Example Illustrations
- **Example 1**: Demonstrates the scenario where only the first four covariates are relevant for classification in a normal distribution setting.
- **Example 2**: Showcases the limitation of existing screening methods in detecting signals through bivariate distributions when marginals are identical.

## Problem Statement
Traditional methods have limitations, such as including noise variables in the screened set and failing to detect signals from identical marginal distributions. The accumulation of noise variables deteriorates classification accuracy and becomes more severe with increasing sample size.

## Proposed Method
We propose a model-free screening method for binary classification problems in ultrahigh-dimensional settings that:
1. Retains only relevant features, eliminating noise variables without relying on a specified model.
2. Detects pairs of features with significant interactions using Peacock's test, a two-dimensional extension of the Kolmogorov–Smirnov test.
3. Ensures coherence between the screening process and the discriminant used for classification by employing Kernel Density Estimation (KDE) and Kernel Discriminant Analysis (KDA).

## Objectives
The project aims to:
- Address the high-dimensional classification problem by eliminating irrelevant covariates.
- Introduce a novel screening method that outperforms existing techniques.
- Provide a coherent approach to feature selection and subsequent classification.

## Repository Usage
[To be updated]

## Contact
For queries or further information, please contact  [shlokm21@iitk.ac.in].

