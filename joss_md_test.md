---
title: 'NMAcompanion: an R package for nuclear morphology analysis graphing and statistical tests.'
tags:
  -R
  -Sperm
  -morphology
  -morphometrics
  -fertility
authors:
  - name: Joe Perry
    orcid: 0000-0000-0000-0000
    affiliation: 1
  - name: Ben Skinner
    orcid: 0000-0000-0000-0000
    affiliation: 1
affiliations:
  - name: University of Essex,UK
    index: 1
date: 13 May 2025
bibliography: paper.bib
---
# Summary 

NMAcompanion is an R package that provides a simple analytical framework and graphing pipeline for the analysis of the many subtle differences between nuclear shape within sperm. The input data for NMA companion is the full stats data produced by the programme NMA (put reference here to NMA). NMA performs automated characterisation of nuclei from fluorescence images that creates the previously mentioned stats data. The package is made up of one main pipeline and some convenience functions. The main pipeline entails three steps: the detection of parts of the nuclei in the dataset that are multiple different shapes, the clustering of those different shapes in relation to the different nuclear and the association of those shapes with more specific phenotypes. There are also convenience functions to load data and build graphs that help describe the morphology. The produced phenotypes are useful in the characterisation of different biological and experimental phenomena in a nuclear morphology dataset.

# Statement of need 

This software acts to streamline the further development of the usage of nuclear morphological analysis in the study of fertility. It provides access to greater statistical analysis for sperm morphology and acts as a platform for further research of the sperm morphology of different species and that of well characterised species in various experimental conditions. This cluster and feature-based analysis of shape will help find subtle morphological phenotypes and allow for more in-depth association of morphology and the biological conditions that affect spermiogenesis. This is useful in the study of nuclear morphology as it allows for the rapid processing of images with the separation of automated imaging artifacts. It does this by providing groups of nuclei with differing morphology to the general shape of the dataset that can then be removed if they are errors or considered if they represent real morphological variation. It also solves the issue of cells with two distinct mirror image orientations that make their morphological variance much more continuous and so feature detection requires an orientation independent analysis. Within the Skinner lab at University of Essex morphological analysis is commonplace and this software was developed as part of a continued effort to streamline and automate various parts of this process. It is difficult to compare to other software as it fits a very specific niche, there is no other software package designed to support the morphological analysis platform NMA. Too our current knowledge the kind of in depth analysis of the subtle morphological differences in sperm do not exist in literature. The other analytical processes for assessing sperm head morphology are mostly either manual or use simpler morphological measures and so cannot be directly compared.

# Usage

The functions and statistical methods employed in NMA companion were originally produced for the paper (put reference to the pig paper here) as well as (reference MSD thesis here). During that study they were used to detect and characterise the subtle morphological variation within pig sperm morphology. NMAcompanion allowed us to find out that pig sperm has two distinct mirror image orientations that weren’t superimposable due to slightly different shapes. The phenotypes being linked to orientation caused us to have to look for orientation independent analysis prompting the development of the detection of orientation independent features in NMA companion. Once these two mirror image phenotypes were accounted for further analysis showed that previously found morphological phenotypes could be re-contextualised with the analysis system. In a practical sense as seen in figure 1 this software can be easily used either by running the wrapper function or by running the analysis step-by-step.

![Figure 1: a full flowchart of the various steps taken in an NMAcompanion analysis. The data is first loaded from a file exported from NMA. Regions of interest on the nuclei are detected with the get region of interest function. Clusters of informative features are found with the clustering functions. There are then to perhaps graphs can be generated to describe those clusters and those clusters can be converted into morphologically informative IDs representative of the features of the nuclear that are then amalgamated by Hamming distance to produce consensus images of each of the phenotypes within a dataset.\label{fig:example}](nma_companion_flowchart.png)
 



