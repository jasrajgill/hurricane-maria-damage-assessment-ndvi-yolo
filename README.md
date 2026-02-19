**Hurricane Maria Damage Assessment Using Satellite Imagery**
**Project Overview**

Hurricane Maria caused widespread environmental and infrastructural damage across Puerto Rico in 2017. This project leverages Sentinel-2 satellite imagery, vegetation indices, and computer vision techniques to assess the extent of damage before and after the hurricane. By combining NDVI analysis with object detection using YOLO, the analysis identifies areas of severe vegetation loss and structural damage, supporting data-driven disaster response and recovery planning.

**Objectives**

Quantify environmental damage using pre- and post-event NDVI analysis

Identify land areas most affected by Hurricane Maria

Detect and label damaged structures using object detection

Translate satellite data into actionable disaster response insights

**Dataset**

Satellite Source: Sentinel-2 imagery

Data Type: Pre- and post-hurricane multispectral images

Labels: Manually annotated damage indicators (25+ images)

Tools Used for Labeling: RoboFlow

**Tools & Technologies**

Python

Jupyter Notebook

NumPy, Pandas

Rasterio, OpenCV

YOLO (Object Detection)

Matplotlib / Seaborn

Satellite Image Processing

Methodology

NDVI Analysis

Computed NDVI for pre- and post-hurricane images

Visualized vegetation loss across multiple land regions

Compared spatial NDVI changes to identify severely impacted areas

Damage Rationale

Analyzed geographic and environmental factors contributing to uneven damage

Examined elevation, land cover, and storm exposure patterns

**Object Detection**

Labeled damaged structures in satellite imagery

Prepared datasets for modeling

Trained a YOLO model over 50 epochs to detect damage indicators

**Key Insights**

Significant NDVI decline was observed in coastal and low-lying regions, indicating severe vegetation damage.

Mountainous regions showed relatively lower NDVI loss, suggesting natural resilience due to elevation and vegetation density.

The YOLO model successfully identified damage patterns in post-event imagery, demonstrating the feasibility of automated disaster assessment.

Combining NDVI and object detection provides a more comprehensive understanding than either approach alone.

**Actionable Recommendations**

Prioritize recovery efforts in regions with the highest NDVI loss to restore agricultural productivity.

Deploy satellite-based monitoring as a rapid assessment tool immediately after disasters.

Integrate object detection models into emergency response workflows to identify damaged infrastructure at scale.

**Future Improvements (3-Month Roadmap)
**
Increase labeled image volume to improve model accuracy.

Incorporate elevation and weather data for multi-layered impact analysis.

Experiment with alternative deep learning architectures for improved detection.

Automate the NDVI and detection pipeline for real-time disaster monitoring.

**Sources**

Sentinel-2 Satellite Documentation

NASA Earth Observatory

FAO NDVI & Remote Sensing Resources
