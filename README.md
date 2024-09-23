# seoMLpipeline
Proof of Concept Pipeline using SEO data to identify areas of opportunity to drive website traffic

Originally built in Google Colab, the Python Script:
  - creates a drive location for a dataset of SEO data
  - builds a PySpark pipeline to impose a schema of features on that dataset
  - runs a FAISS clustering algorithm on that data to create a new feature set
  - performs feature engineering to create new KPIs from available data to identify opportunities
  - interpolates missing values either by the mean or by sampling from the distribution to preserve data integrity and scale
  - one hot encodes SERP features for algorithm use
  - and builds several machine learning model options - both regression and classification - that would be preferable depending on the business context

Separately in R, we created visualizations of the dataset to identify opportunity areas in more easily interpretable ways.

Disclaimer: This was originally real business data and has been shuffled to preserve asset value. All model results solely represent a proof of concept.
