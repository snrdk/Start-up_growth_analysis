# Start-up Growth Analysis
This repository contains several R scripts that implement various functions for data analysis, clustering, and estimation algorithms. This code uses a Bass diffusion model to fit a startup's monthly active users (MAUs) and estimate growth to compare the parameters of each. 

🐤 **Related work**: Lee, S., Kim, J., & Lee, H. (2025+). Unveiling the types of growth patterns of mobile startups: do business models matter? Technology Analysis & Strategic Management, In press. https://doi.org/10.1080/09537325.2024.2361420.


## Bass diffusion model
<div align="center">
    <img src="https://github.com/user-attachments/assets/58846f93-ff24-4ff9-b338-7c5d2bd4270e" alt="image00016" width="600"/>
    <p style="font-size: 14px; color: gray;">Image source: NumberAnalytics</p>
</div>  

The Bass diffusion model is a predictive tool used to forecast the adoption of new products or innovations over time. It models the process by which new products are adopted in a population.

Key coefficients:  
* p (coefficient of innovation): Represents the likelihood of adoption by innovators or early adopters, independent of social influence.
* q (coefficient of imitation): Represents the likelihood of adoption based on social contagion or word-of-mouth influence from previous adopters.
* m (market potential): The total number of potential adopters in the market over the product's lifetime.



## Usage

Each R file provides functions for different types of analysis and modeling. For example, the `Cluster_analysis.R` script contains functions to perform cluster analysis, while `Bass functions.R` provides functions for Bass diffusion modeling.

**Part of the results**
<div align="center">
    <img src="https://github.com/user-attachments/assets/27c5a82f-8c1b-43b6-b373-431741eb6019" alt="ctas_a_2361420_f0003_oc" width="1200"/>
</p>
</div>


## Key Features
* **Estimation** Algorithms: Perform various estimations based on input data.
* **Cluster Analysis**: Identify and analyze different clusters within the dataset.
* **Bass Modeling**: Implement the Bass diffusion model to forecast market adoption.

The scripts included in this repository are:

- `fixed_estimation.R`: Implementation of estimation algorithms
- `tg_base_functions.R`: Collection of base functions
- `Bass functions.R`: Functions for the Bass diffusion model
- `Cluster_analysis.R`: Functions for cluster analysis
- `crosstable.R`: Functions for crosstable analysis
- `Estimation.R`: Additional estimation algorithm implementations
