---
title: 'tSNEr: An open-source, interactive tool to perform and visualise t-SNE for flow cytometry in R'
tags:
  - R
  - flow cytometry
  - dimension reduction
authors:
  - name: Chen Liu
    orcid: 0000-0002-5693-8634
    affiliation: "1, 2, 3"
affilications:
  - name: Department of Medicine, Université de Montréal, Canada
    index: 1
  - name: Deaprtment of Microbiology, Infectiology and Immunology, Université de Montréal, Canada
    index: 2
  - name: Cell Signalling in Immunotherapy Research Unit, Hospital Maissonneuve-Rosemont Research Centre (CRHMR), Canada
    index: 3
date: 25 September 2025
bibliography: paper.bib

---

# Summary

tSNEr is an open-source R package for processing, calculating and visualising t-distributed stochastic neighbour embedding (t-SNE) for flow cytometry data. This tool is simple to use and functions offline.

# Statement of need

Flow cytometry has become an indispensable tool for single-cell analysis in immunology, haematology, and cell biology research, generating high-dimensional datasets that require sophisticated computational approaches for interpretation. While t-SNE has emerged as a leading dimensionality reduction technique for visualising cellular heterogeneity and identifying distinct populations in single cell RNA sequencing or CyTOF, its application to conventional flow cytometry analysis presents unique challenges and opportunities that remain underserved by existing tools. Commercial platforms offer robust analysis capabilities, but require subscription fees that can be prohibitive for certain research groups. The tSNEr provides a free, integrated, interactive R-based workflow that streamlines the entire analysis pipeline: from FCS file import and marker selection to parameter optimisation and visualisation—within a single, intuitive interface. By automating file handling, offering intelligent parameter defaults based on dataset characteristics, and providing web-based visualisations, tSNEr enables researchers to focus on results interpretation rather than computational implementation.

# Data import, preprocessing and quality control

The tSNEr pipeline begins with automated detection and loading of Flow Cytometry Standard (FCS) files from the working directory. Files are imported using the flowCore package without automatic transformation to preserve original measurement scales (B. Ellis et al., 2025). The expression matrix is extracted directly from the FCS file structure, maintaining the original dimensions of n cells × p markers, where each row represents a single cell and each column represents a fluorescence parameter or scatter measurement. Marker annotations are retrieved from the FCS parameter descriptions when available, with automatic fallback to parameter names for channels lacking descriptive metadata. This dual-extraction approach ensures complete marker identification across different FCS file formats and export configurations from various cytometer manufacturers.

Following data import, an interactive marker selection interface allows users to specify relevant parameters for dimensionality reduction analysis. The selection process supports multiple input formats including individual selection, comma-separated lists, and range notation, providing flexibility for different experimental designs. Selected markers undergo quality control filtering where cells containing missing or invalid measurements (NA, NaN, or Inf values) are automatically removed to ensure computational stability. The pipeline requires a minimum of 30 cells for meaningful t-SNE analysis, consistent with perplexity constraints in the underlying algorithm.


# Data transformation, normalisation and parameter optimisation

Flow cytometry data undergoes arcsinh transformation with a cofactor of 5, a standard approach in the field that addresses the unique characteristics of fluorescence measurements (Finak et al., 2010). This transformation provides quasi-linear behaviour near zero to preserve resolution of dim populations while exhibiting logarithmic properties at higher intensities to accommodate the broad dynamic range typical of flow cytometry data. Importantly, this transformation gracefully handles negative values that arise from compensation procedures. Following transformation, each marker is independently scaled to zero mean and unit variance using z-score normalisation. This standardization ensures equal weighting of all markers in the subsequent dimensionality reduction, preventing high-intensity markers from dominating the distance calculations. The pipeline implements intelligent default parameter selection based on dataset characteristics while allowing user override for specialized analyses. Perplexity, which controls the effective number of neighbours considered for each point, is automatically calculated as **equation 1**:
$$\textit{perplexity} = \min\left[30, \max\left(5, \sqrt{\frac{n}{100}}\right)\right]$$ (eq 1)
where n represents the total number of cells from all files. This formulation ensures appropriate local neighbourhood sizes across diverse dataset scales, from small pilot experiments to large-scale profiling studies (Pedregosa et al., 2011). The learning rate (η) is determined as 200 in default to maintain stable convergence. The Barnes-Hut approximation parameter θ is set to 0.5, balancing computational efficiency with embedding accuracy. Initial dimensionality reduction via principal component analysis (PCA) as 50 components accelerates computation and improves optimisation, stability, and is a standard practice in t-SNE implementations (Luecken and Theis, 2019). As shown in \label{fig:1}, tSNEr automatically calculate perplexity and leaning rate based on the total number of cells from all input files.

![Fig 1. program generated default parameters when running two FCS files with total number of cells as 20,000\label{fig:1}](https://github.com/liuchen37/Pics/blob/main/Figure1_JOSS_LIU_2025.jpg?raw=true)

# T_SNE algorithm implantation
The t-SNE algorithm proceeds through several computational stages to generate the final two-dimensional embedding. Initial dimensionality reduction via PCA projects the high-dimensional data onto the first 50 principal components, reducing noise and computational complexity while preserving global structure. High-dimensional similarities are computed using Gaussian kernels with adaptive bandwidths. For each cell i, the bandwidth σ[i] is selected such that the perplexity of the conditional distribution equals the user-specified value, where perplexity is defined as equation 2:
$$\textit{Perplexity} = 2^{H(P[i])}$$ (eq 2)
with H(P[i]) representing the Shannon entropy of the conditional distribution P[j|i]. These conditional probabilities are symmetrised to obtain joint probabilities: 
$$P[i, j] = \frac{P[j|i] + P[i|j]}{2n}$$ (eq 3)
The Barnes-Hut algorithm constructs a spatial tree structure enabling O(n log n) computation of gradient approximations. Points sufficiently distant (determined by the ratio of node size to distance exceeding θ) are treated as single entities, dramatically reducing computational requirements for large datasets. Procedural optimisation proceeds via gradient descent with momentum to minimize the Kullback-Leibler (KL) divergence between high-dimensional similarities P and low-dimensional similarities Q (equation 4). The low-dimensional similarities employ Student's t-distribution with one degree of freedom: 
$$Q[i, j] = \frac{(1 + \|y[i] - y[j]\|^2)^{-1}}{\sum_{k \neq i} (1 + \|y[k] - y[i]\|^2)^{-1}}$$ (eq 4)
where y[i] represents the low-dimensional coordinates of cell i (Kobak and Berens, 2019, van der Maaten and Hinton, 2008). The optimisation schedule implements early exaggeration (multiplying P by 12) for the first 250 iterations to promote cluster separation, followed by standard optimization for the remaining iterations (Kotecha et al., 2010). Momentum increases from 0.5 to 0.8 after iteration 250 to accelerate convergence while maintaining stability (Cai and Ma, 2022).

The pipeline implements several optimizations to handle the scale and complexity of modern flow cytometry experiments. Memory-efficient processing strategies automatically subsample datasets exceeding 100,000 cells while preserving population representation through density-dependent sampling. Parallel processing capabilities enable simultaneous analysis of multiple samples when computational resources permit. Error handling routines monitor convergence behaviour, alerting users to potential issues such as increasing KL divergence or numerical instabilities. Parameter validation ensures mathematical constraints are satisfied (e.g., perplexity < n/3) before initiating computationally intensive operations.

A complete walkthrough of the implementation pipeline was shown in \label{fig:2}. This output captures the complete t-SNE dimensionality reduction workflow for 20,000 flow cytometry cells with 3 markers. The algorithm begins by scaling and cantering the data, then performs PCA reduction to accelerate computation. The preprocessing phase establishes the similarity structure, computing pairwise distances between cells and converting them to probability distributions using a perplexity of 14 (automatically calculated to balance local and global structure preservation). The Barnes-Hut tree construction (theta = 0.5) enables efficient approximation of these similarities, processing all 20,000 points in under 9 seconds. During the learning phase, the algorithm iteratively refines cell positions through gradient descent optimization with learning rate η = 200. The Kullback-Leibler divergence drops dramatically from 116.27 at iteration 50 to approximately 2.92 by iteration 1000, indicating successful convergence. The error stabilizes around iteration 500 (error ≈ 2.70), with subsequent iterations showing minor fluctuations between 2.0-2.9, typical of stochastic optimization reaching equilibrium. Each 50-iteration block maintains consistent computational efficiency (~1.4-1.7 seconds), demonstrating the scalability of the Barnes-Hut implementation. The total runtime of 29.66 seconds for 1000 iterations confirms t-SNE's feasibility for analysing moderate-sized cytometry datasets, with the final low error value indicating faithful preservation of local cellular neighbourhoods in the two-dimensional embedding.

![Fig 2. t-SNE Dimensionality Reduction Opitmisation Process for Flow Cytometry. Console output of t-SNE algorithm by tSNEr reducing 20,000 cells from 3D to 2D space. KL divergence error decreases from 116.3 to 2.9 over 1000 iterations (29.66s runtime), indicating successful convergence.](https://github.com/liuchen37/Pics/blob/main/Figure%202_JOSS_LIU_2025.jpg?raw=true)

# Application example: tSNEr reveals T cell developmental stages in murine thymus

The package tSNEr successfully captures the well-characterized developmental trajectory of T cells in thymic tissue, revealing distinct populations based on CD3, CD4, and CD8 expression patterns (\label{fig:3}). The analysis clearly delineates four major populations: CD4+CD8+ double positive (DP) cells forming the largest cluster, CD4+CD8- and CD4-CD8+ single positive mature T cells in separate regions, and CD4-CD8- double negative (DN) precursors. The CD3+ population (red arrow, panel A) are predominantly CD4 (blue arrow) or CD8 (orange arrow) single positives. 

![Fig 3. A: Murine thymic profiling generated by tSNEr; B: T cell maturation pathway](https://github.com/liuchen37/Pics/blob/main/Figure%203_JOSS_LIU_2025.jpg?raw=true)

# Acknowledgements

The development of tSNEr involves prestigious contribution provided by several foundational R packages, including base64enc (Urbanek, 2012), Rtsne (Krijthe, 2014), dplyr (Wickham et al., 2014a), tidyr (Wickham et al., 2014b), flowCore (B. Ellis et al., 2025), ggplot2 (Wickham, 2009), gridExtra (Auguie, 2010), htmltools (Cheng et al., 2014) and RColorBrewer (Neuwirth, 2002).  

# References
