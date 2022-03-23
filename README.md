# 2022 State of the State Text Analysis
The project compares 18 Republican governors' speeches aggregated together to the 18 Democrat governors' speeches. The main goal of this paper is to see if climate change is a topic of conversation, and if not, what is? In addition, is there a different between the two political parties' governments' speech topics?

# Data 
2022 speeches of 18 states in each party. 
Data is pdf downloads of each states governors' speech. They were all converted to csv's - each line being a sentence - and csv's merged together per group.

# Installation
You can download the data in both pdf and csv form. The pynotebook used to convert from pdf to csv and merging has been uploaded thus can be used to add other states/speeches that are in pdf forms.

The code used to derive the insights that are in pdf form are also accessible.

# Usage

It can be seen that climate change and clean energy is mentioned much more in Democratic governors' speeches, however, both groups' speeches seemingly tend to be centered around other topics. 

Overall, the speeches of governors between the two parties is more or less very similar with a 0.865 cosine value. Both parties tend to talk about healthcare, education, economic development and taxation. Republicans tend to talk more about law enforcement and the nation and mention their state names in their speeches a lot more compared to Democrats who tend to talk more about housing and transportation.

# Roadmap
Lemmatization has many errors, likely leading to over counting/under counting of certain words and phrases. This mmost likely led to mistaken topic modelling and bigram analysis. Better preparation of data likely will assist.

Despite these setbacks, this code can be used to compare different text groupings that don't necessarily have to be political in nature.
