#!/bin/bash

# Script to get NatLog polarity annotations from CoreNLP.
# Up-to-date Stanford CoreNLP jars can be downloaded from https://stanfordnlp.github.io/CoreNLP/index.html#download.
# This only requires the primary corenlp jar and the corenlp models.

#CP=".:~/research/CoreNLP/stanford-corenlp.jar:~/research/CoreNLP/lib:~/research/CoreNLP/liblocal:stanford-corenlp-models-current.jar:stanford-english-corenlp-models-current.jar:stanford-english-kbp-corenlp-models-current.jar:stanford-corenlp-full-2018-02-27/stanford-corenlp-3.9.1.jar"

JAR_PATH="jars"
CORENLP_VERSION="3.9.1"

CP=".:${JAR_PATH}/stanford-corenlp-${CORENLP_VERSION}.jar:${JAR_PATH}/stanford-corenlp-${CORENLP_VERSION}-models.jar"

javac -cp $CP  PolarityTest.java

if [ $# -eq 0 ]; then
  java -cp $CP PolarityTest
elif [ $# -eq 2 ]; then
  java -cp $CP PolarityTest $1 $2 
else
  echo "Invalid number of arguments."
fi
