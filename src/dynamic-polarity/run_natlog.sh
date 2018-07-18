# Script to run NatLog from CoreNLP.

#CP=".:~/research/CoreNLP/stanford-corenlp.jar:~/research/CoreNLP/lib:~/research/CoreNLP/liblocal:stanford-corenlp-models-current.jar:stanford-english-corenlp-models-current.jar:stanford-english-kbp-corenlp-models-current.jar:stanford-corenlp-full-2018-02-27/stanford-corenlp-3.9.1.jar"

JAR_PATH="jars"
CORENLP_VERSION="3.9.1"

CP=".:${JAR_PATH}/stanford-corenlp-${CORENLP_VERSION}.jar:${JAR_PATH}/stanford-corenlp-${CORENLP_VERSION}-models.jar"

javac -cp $CP  PolarityTest.java
java -cp $CP PolarityTest
