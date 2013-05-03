#!/usr/bin/python

"""
Created on Jan 13 , 2013
Filename: kNN.py
@author : libo
"""

from numpy import *
from sys import argv
from os import listdir

#scritp , filename = argv

def importData():
	group = array([[1.0 , 1.1 ],[1.0 , 1.0],[0,0],[0,0.1]])
	labels = ['A' , 'A' , 'B' , 'B']
	return group , labels

def classify(inX , trainSet , labels , k):
	dataSize = trainSet.shape[0]
	diffMat = tile(inX , (dataSize ,1)) - trainSet
	sqDiff = diffMat**2
	sqDistances = sqDiff.sum(axis = 1)
	distances= sqDistances**0.5
	sortedIndex = argsort(distances)
	classCount = {}
	for index in range(k):
		xlabel = labels[sortedIndex[index]]
		classCount[xlabel] = classCount.get(xlabel, 0) + 1
	#print classCount
	sortedClassCount = sorted(classCount.iteritems() ,
	key = lambda x:x[1] , reverse = True)
	#print sortedClassCount
	#print "result is : " , sortedClassCount[0][0]
	return sortedClassCount[0][0]

def importData_M(filename):
	fr = open(filename)
	numOfLines = len(fr.readlines())
	dataMat = zeros((numOfLines , 3))
	classLabels = []
	fr = open(filename)
	index = 0
	for line in fr.readlines():
		line = line.strip()
		lineList = line.split('\t')
		dataMat[index , :] = lineList[0:3]
		classLabels.append(lineList[-1])
		index = index +1
	return dataMat , classLabels

def autoNorm(dataSet):
	#print dataSet
	maxVals = dataSet.max(0)
	minVals = dataSet.min(0)
	ranges = maxVals - minVals 
	m = dataSet.shape[0]
	normData = zeros(shape(dataSet))
	diffData = dataSet - tile(minVals , (m , 1))
	#normData = tile(ranges , (m , 1))
	normedData = diffData/tile(ranges , (m ,1))
	#print normedData
	return normedData 

def classTest():
	hoRatio = 0.1
	dataMat , dataLabels = importData_M("datingTestSet.txt")
	normedMat = autoNorm(dataMat)
	m = normedMat.shape[0]
	testNum = int(m * hoRatio)
	errorCount = 0.0
	for i in range(testNum):
		classifyLabel = classify(normedMat[i,:] , normedMat[testNum :m ,:] , dataLabels[testNum : m] , 5)
		print "the classify come back with %r , the real labels is %r " %(classifyLabel , dataLabels[i])
		if (classifyLabel != dataLabels[i]) :
			errorCount += 1.0
	print "the error rate is: %f" %(errorCount/float(testNum))

def imgVector(filename):
	returnVec = zeros((1,1024))
	fr = open(filename)
	for i in range(32):
		lineStr = fr.readline()
		for j in range(32):
			returnVec[0 , 32*i+j] = int(lineStr[j])
	return returnVec

def handWriteTest():
	hwLabels = []
	trainingList = listdir("trainingDigits")
	m = len(trainingList)
	trainingMat = zeros((m , 1024))
	for i in range(m ):
		filenameStr = trainingList[i]
		fileStr = filenameStr.split(".")[0]
		classLabel = int(fileStr.split("_")[0])
		hwLabels.append(classLabel)
		trainingMat[i,:] = imgVector("trainingDigits/%s"\
		%filenameStr)
	testFileList = listdir("testDigits")
	errorCount = 0.0
	mTest = len(testFileList)
	for i in range(mTest):
		testFile = testFileList[i]
		testStr = testFile.split(".")[0]
		testLabel = int(testStr.split("_")[0])
		testVector = imgVector("testDigits/%s" \
		%testFile)
		classResult = classify(testVector ,trainingMat , \
		hwLabels , 3)
		print "classify come back :%d , the real answer is :%d\
			" %(classResult , testLabel)
		if ( classResult != testLabel):
			errorCount +=1
	print "the total error num is : %d" %errorCount
	print "the error rate is : %f" %(errorCount/float(mTest))
 	 

if __name__ == '__main__':
#	group , labels = importData()
#	group , labels = importData_M("datingTestSet.txt")
#	group = autoNorm(group)
#	classify([0,0 , 0] , group , labels , 3)
#	classTest()
	handWriteTest()
