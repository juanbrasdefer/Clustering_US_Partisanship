# Clustering_US_Partisanship

### Summary
Data Science 'Clustering' algorithms, especially when implemented as mutually validating approaches, have the power to reveal (albeit do not name) latent associations between voting blocs when shown Electoral and Demographic Data. Though special care should be taken in drawing direct conclusions from the results, the value of these methods comes in elucidating avenues for investigation and helping to rethink the current understanding of voter characteristics, profiling, and behaviour in presidential elections.


### Research Objective
Up until the presidential election of 2016, there existed commonly-held beliefs about the nature of certain states and their voting patterns, such as the famous idea of the "Blue Wall" states (Kennedy, 2018). In the wake of Clinton's loss in 2016, however, it became very clear that understanding voters and their preferences was not to be taken for granted. 
However, the United States is a diverse and complex country. 
While factors such as gender and race have traditionally been heavily correlated with party choice (e.g. women tend to vote more Democratic than men) (Gilens, 2023), the migration of many demographic groups across partisan lines in 2016 and 2020 have shown a dynamism that is far from static. This creates a necessity for a re-imagining of the manner in which voter profiling is thought of, and especially the ways in which latent traits may explain the behaviours of seemingly disparate blocs of voters. 

The objective of this study is to gain insights into the way that demographic and socioeconomic characteristics interact with voter partisanship in the United States. By looking at presidential election voting results and American Community Survey data, the study aims to explore the multi-dimensional segmentation possibilities that exist within the population and the latent traits that bind certain groups together and explain their electoral behaviour. By elucidating these relationships, more informed and targeted policy proposals can be made by parties and candidates.

### Data
This study makes use of two datasets, dealing with voting and census data respectively.

#### Dataset 1: Voting Data
The voting data comes from the MIT Election Data and Science Lab's "County Presidential Election Returns 2000-2020" dataset (MIT Election Data and Science Lab, 2024). This dataset details the county-level vote counts in each 4-year presidential cycle, disaggregated by party. The dataset includes all ~3100 counties, and these are used in the first experiment.


The primary variable of interest for this study is the margin by which a given county had a majority-Democrat (or majority-Republican vote), henceforth called "Net Vote Margin" (NVM). This variable is a simple composite variable created by subtracting the proportion of republican votes (relative to total votes) from the proportion of democrat votes (ie: NVM = (Dem/Total) - (Rep/Total)). 


#### Dataset 2: Census Data
The census data comes from the United States Census Bureau's "American Community Survey (ACS)" (U.S. Census Bureau, 2024). The ACS is a federal survey conducted on a semi-continuous, monthly basis, that details demographic, economic, and social aspects of communities in the United States.
Among the ~250 distinct variables available in the ACS, only a relevant subset will be used in the clustering analysis of this study. These variables are:

Education: 		% of population with min Bachelor
Immigration:	 	% of population foreign-born
Income: 		Median income, Normalized (minmax)
Unemployment: 	% of population unemployed
Marital Status:	 	% of males never married

Education is chosen for its understanding as having a strong effect on both turnout of the population in elections as well as for the partisan leaning of its voters (Ahearn et al, 2023). Immigration is an important factor in the United States specifically because of the high number of immigrants that come to the country each year and for the way that this immigration and the 'substance' (skill level, race...) of this immigration affects the voting behaviour of the native population (Mayda et al, 2022). Income is selected for the increased economic disparity that plagues the United States (Gelman et al, 2010) and the topic's centrality in federal electoral campaign platforms (Gelman et al, 2010). Unemployment is chosen for its unique and often decoupled behaviour relative to income and its localized tendency when compared to the performance of the national economy (Part et al, 2020). Finally, Marital Status is taken as a proxy for the crucial 'gender divide' in political affiliation that has come into existence in the United States since 2016 (Philpot, 2018).



### Techniques Applied
Two techniques are used; both fall under the category of 'Data Science Clustering Techniques'. 
Clustering in Data Science attempts to uncover underlying 'groupings' amongst observations. By taking variables common across the observations, these clustering algorithms create subsets of a dataset that can be used for further analysis or exploration.
In this study, the clustering techniques are employed with the purpose of exploring the different ways in which counties can be characterized and the latent associations that tie them together.

The first of these techniques is a slight variation of the long-standing KMeans algorithm (Perez-Ortega et al, 2018), "KMeans++". This algorithm is known as a 'prototype-based method', which means that the algorithm assign data points to clusters via a 'physical' (graphical) distance measure, originating from a 'centroid' (central point). This kind of algorithm typically does well with linear data, and the KMeans++ variation specifically helps to avoid poor centroid-initialization, which is useful when there are many variables present (as in this study).

The second technique is "Density-Based Scan (DBScan)" (Ester et al, 1996). It was specifically designed to deal with 'noise' points in the data and datasets that may have less-conventional spreads (shapes) when compared to approaches such as KMeans. This algorithm serves as an excellent complement to the centroid-based algorithms as it is capable of identifying clusters of non-globular nature, as well as being less sensitive to outliers. As well, the additional computational expense required by DBScan's pairwise calculations are mitigated in this study due to the relative brevity of the dataset.

The central idea behind using two different clustering methods is to utilize their results as a sort of 'validation' of each other. By looking at the clusters that each of the algorithms assigns to any given data point, an additional layer of analysis is unlocked. This approach to validation also serves as the jumping-off point for analyzing the results, whereby counties that are found to have interesting cluster assignments by both algorithms are given precedence in scrutiny.


