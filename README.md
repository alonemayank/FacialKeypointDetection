# FacialKeypointDetection
## Introduction
**Problem Description:** The main problem to be addressed is to train the application with machine learning capabilities using training dataset for detecting facial key points such as right eye center, nose tip, lip corners, mouth corners and other points. The complexity arises due to heterogeneous facial features such as 3D pose, size, position, and viewing angle. 
**Motivation:** Facial recognition is very popular biometric technique these days. Various developments has already been observed in facial recognition technologies, but there is still a huge scope and need of improvement. So, motivation for this project came from the use of this application as building block in several applications such as tracking faces in images and videos, Analyzing Facial Expressions, Detecting Dysmorphic Facial Signs for Medical Diagnosis and Biometrics/ Face Recognition.
It has always been difficult to find out facial key points as each individual face differs from other face in many features. It is the major challenge.

**Report Organization:** This report covers below topics. 
1) Data Exploration: This section contains information about dataset we have used and more details about the attributes and records.

2) Methodology: This section contains two sections, data preprocessing and data mining. Data preprocessing contains information about techniques used to clean and prepare data for training. Data mining contains information about algorithms used to build the models, detailed information about these algorithms and the performance of these algorithms.

3) Logic of Problem: This section contains information about Logic of Problem and pros and cons of thinking.

4) Conclusions: This section contains information about final conclusions based on all above processing’s and summary of the problem.

## Data Exploration
### Summary:
Data for our project has been obtained from Kaggle’s website (https://www.kaggle.com/c/facial-keypoints-detection/data). The data set consists of two .csv files – training.csv and test.csv. With the training.csv file’s data we train our algorithm. The test.csv file, which has data of face images, will be used to test the algorithm.

Training.csv: This file consists of uncleansed 7049 entries and each row of the 7049 entries contain (x, y) coordinates of 15 key points for example left_eye_center_x, left_eye_center_y, right_eyebrow_outer_end_x and so on.

Test.csv: This file has 1783 uncleansed test images .Since the images are in 96x96 pixel format each row contain 9216 (96*96) entries which are pixel value between 0 to 255. To visualize this the file has to be cleaned.

Attribute: There are 31 attributes in total. X-Coordinates and Y-Coordinates each for 15 facial key points and last attribute is of image on which these key points will be tested.
Data type: All the attributes have numeric data type. And as name suggests, left_eye_center, it represents left eye center point coordinate and same for other 14 attributes.
Methodology

### Data Preprocessing:
Original data obtained from Kaggle was difficult to process and understand as the csv file which was huge in size and MS-Excel was failing to load the data accurately. We later found out that image attribute was in string format in both training.csv and test.csv file. This data is about the bits of image for rendering purpose. As can be seen from image attribute, it’s difficult to understand and confusing. Below preprocessing techniques were applied to clean the data for further processing.
Attribute Separation: Training dataset contains image attribute which has information of each pixel of image. We separated this attribute from other attributes. This separation could be done in R and MS Excel. We chose Excel as it is quite easy to separate data in Excel than in R. This separation helped us in understanding each attribute clearly and in visualizing or locating missing values and anomalies in data.
Missing Values: There were many missing values in the training data set. .We replaced these values in Weka using filter ReplaceMissingValues as it replaces values with mean values While using this filter, we did not consider image attribute as it didn’t have any missing value. In facial key point dataset case, the best approach would be to replace missing values with the mean of co-ordinates values which can match with majority of images and their facial keypoints.
Feature Subset Selection: While separating the image attribute, new attribute ‘ImageNo’ was created for better understanding. This does not play any role in building the model, hence it was removed.
Other: Lower part of training data had majority of missing values, and we replaced these with mean value. Later we dropped these lower rows while applying algorithms otherwise result would have been biased because of the replaced mean values. We did not use discretization for our dataset as each attribute is an individual facial key point which cannot be categorized further.

### Data Mining:
**Models:** It is clear with the dataset that our project requires estimation algorithms to be applied. We did research to select the popular applicable methods for estimating or processing image/object estimation problems. We finally came up with three major algorithms for our project:
Linear Regression Algorithm
Multilayer Perceptron (Neural Network)
Image Patch Method
Individual Model:
**Linear Regression Model:** 
**Summary:** Linear Regression algorithms are applicable to estimate attribute by calculating other attribute’s contribution to it. This algorithm plots the best fit line to estimate the desired attribute. We can select the desired attribute in Weka to draw the formula of best fit line with other attributes. 
Parameterization: Linear Regression Model requires all attributes to calculate their weight into the final equation to derive or predict the desired attribute.
Conclusion: Linear Regression Model is a good choice when it comes to estimate the numeric values from numeric attributes. The main drawback of this algorithm is it fails in case of outliers present in the data.

**Image Patch Method:**

**Summary:** This method builds the average patch of image by selecting a keypoint on the image and with strong capabilities of R, we can visualize that average patch. It then calculates the best score by moving that patch over the overlapping test image with the pixel range given by the user and eventually comes up with the best scored result. This best score then tells us the best co-ordinates of the desired facial keypoint. 
Parameterization: We need to provide this method with the desired range of pixels while calculating the average patch. It also requires the number of pixels to deviate the image patch to calculate the best score for estimating co-ordinates. We have taken 2 pixels deviation to calculate the best score.
Conclusion: Due to the image render capabilities of R, Image patch method produces good desirable results and these results can be visualized. The challenge in this method is to find optimal no of pixels in calculating average mean patch and search space.

**Multilayer Perceptron:**
**Summary:**This is neural network algorithm that works like human brain neurons. Every perceptron is a receptor and transmitter. There are three layers in this network. Input, Hidden Layer and Output layer. Input layer transmit all the attribute to the first hidden layer perceptrons. Hidden layer do most of the work by receiving and transmitting the signals according to the weight depending on the intensity of the input to the perceptron. More number of hidden layers can perform better with the proportional cost of time. Finally output layer have all the attributes to be estimated. Initially these networks use the back propagation method to calculate the error between supplied inputs and obtained outputs. It then calculates the errors backward and adjust weights to input accordingly to learn the relation between the attributes.
Parameterization: Multilayer Perceptron requires to be initialized with the number of hidden layers between input and output layers. Number of optimum hidden layers depend on the result stop improving beyond a significant amount. Input layer takes all attribute except desired output attribute. Weka provides an option to select only one attribute as an output at a time. However MATLAB is capable of building model for estimating all desired output at the same time.


**Conclusion:** Multilayer Perceptron is a good algorithm which can estimate the desired attribute with improvement with the number of iterations. Every iteration improve it’s accuracy and reduce the difference between provided input and desired output. A optimal no of iteration can then be calculated when error stop reducing significantly. The only drawback of this algorithm is time cost being proportional to the no of hidden layers.


### Model Performance:
**Performance Measure Used:**

**Linear Regression:** The performance measure to calculate accuracy of Linear Regression can be calculated with the help of Root Mean Squared Error (RMSE) and Correlation coefficient.

**Image Patch Method:** The performance of this model can be calculated by rendering the coordinates provided by the model based on the score and the number of deviation points selected.

**Multilayer Perceptron:** The performance of multilayer perceptron can be used with the Mean Square Error provided by MATLAB and Weka. Although, fluctuation depends on the number of attributes selected to be estimated in output layer in case of MATLAB. Weight adjusted by back propagation algorithm after each epoch also an important performance measure.

**Meaning of Performance Measure**
Performance measures vary for each algorithm. Correlation coefficient tells us how well predicted outputs are related to actual outputs whereas RMSE is the deviation of dependant values from the regression line. For image patches, comparison is being made by calculating mean of all the patches and superimposing on test images to get the best result
Motivation behind choosing

Linear Regression: Motivation behind choosing the measures is our data being numeric. The standard measure will be Root Mean Squared Error and Correlation Coefficient.

Image Patch Method: The error can be visualized in R for evaluating the accuracy of applied model.

Multilayer Perceptron: The performance for multilayer perceptron can be best estimated using standard Root 
Mean Squared Error (RMSE). In MATLAB, the curve of MSE can be visualized on real time basis to see the improvement with respect to the no of iterations (epochs).

**Estimation of performance measure**

Estimation of Linear Regression model performance depends on the method used to build the model by selecting cross folds and percentage split of training data. No significant changes are observed during change of methods in Linear Regression. It is the same case in Weka for Multilayer Perceptron model but with the fluctuation in no of hidden layers do change the performance. MATLAB uses the percentage split method to measure performance. Image patch method performance depends on level of deviation for pixels to calculate best score of predicted coordinates.
Summary of Performance

The performance used in selected algorithms varies thus there is not a common method to evaluate the inter-algorithm performance. They are quite individual and specific from each other to be evaluated in a tabular format. However we have included available screengrabs to estimate their performance. It can be observed that Multilayer Perceptron method is time taking but quite effective and produce high accuracy results in MATLAB.
	
### Logic of Problem

**Summary of Logic of Problem:** The major factor of this project was the data. Initially, we thought we have sufficient data and some pre-processing will be required (Checkpoint 1). But, our dataset had many missing values and few attributes were in string format on which we performed pre-processing and converted them in a format to be used for applying algorithms. But, this reduced our data size after data exploration (Checkpoint 2). As, our problem was of prediction and not classification, we couldn’t use general known algorithms in WEKA. After some research, we applied Linear Regression and MultiLayer Perceptron. But we were not able to visualize the results, hence we applied algorithms in R as well (Checkpoint 3). Overall, logic of problem was helpful in identifying the mistakes and considering future scenarios. And also to build the plan as how to progress with finding the solution to the problem in hand.  
Pros and Cons: The advantage that logic of problem provides is that it helps us in identifying all the aspects of problem to be solved. It creates complete picture of problem and things to be considered or points of action or what could be the difficulties in solving problem. In our view, the disadvantage is that it has predefined set of headings, but if user wants to add something of it’s own, he cannot.
Recommendations: The analytical thinking tool available is very useful tool to gather thoughts about particular problem. An addition to it could be an option to user to add some heading of it’s own. Also, there should be a section of Methods where user can write about algorithms and approaches that could be used to solve the problem.  

## Conclusions
### **Result Comparison:** 
All three techniques used for building model used same training data, but the results obtained are different. Linear regression yields similarity of almost 90% between predicted value and actual value, but if you consider MultiLayer Perceptron, this similarity is observed as 94%. In case of Image Patch, few facial key points are predicted accurately whereas few points are predicted wrong. This is observed by actually rendering key points on grayscale image in R.
Patterns and Similarity in Models: Techniques applied to build the model were different from each other, so any noticeable patterns were not found. Although, one similarity we noted is that values predicted by Linear Regression and prediction in case of Image Patch using R were in very close proximity.
Better Models: Linear regression predicts key points value but does not consider image attribute, same is the case with MultiLayer Perceptron. On the other hand, Image Patch plays around the image attribute. If we consider all three models, best results were provided by Image Patch technique, then the MultiLayer Perceptron and lastly Linear Regression.

**Problem Summary and Model Conclusion:** The goal of this project was to predict the facial key points on an image. The main challenge was how to use the data. Data can be manipulated in many different ways, based on how you tweak the data, different results are obtained. As result changes, performance is also affected. We did make use of different attributes for different techniques and results were obtained. Hence, it’s difficult to find perfect data with suitable attribute values.

### Future Work:
Image attribute can be considered in case of MultiLayer Perceptron and accordingly changes could be done to the model. Current data contains images of humans only, this can be extended to non-human images as well. Also, more research can be done on algorithms that can be applied using R on image data.
