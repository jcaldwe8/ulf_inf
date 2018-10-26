#!/bin/bash

# Gene Kim 10-22-2018
# Retrieves ULF annotations from the SQL database with a given set of sentence 
# ids and formats it for the inference experiments.  Currently what this 
# currently amounts to is adding the inference annotation ids to link it with
# the inference annotations and cleaning up some extraneous newlines.

WORKING_DIR="get_ulf_temp/"
mkdir ${WORKING_DIR}

# TODO: add option to supply arguments.

# Input ids.
DEFAULT_ANN_DIR="../../../oct-2018-inference-subset/revised-sql-direct-ulf-anns/"
DEFAULT_ULF_ANN_FILE=${DEFAULT_ANN_DIR}"assigned_ulf_anns.txt"
DEFAULT_INF_ANN_FILE=${DEFAULT_ANN_DIR}"assigned_inf_anns.txt"

# Referred dataset metadata.
DATASET_JSON_FILE="../../../oct-2018-inference-subset/source_dataset.json"

# Generated files.
ULF_SQL_REQUEST_FILE=${WORKING_DIR}"request-sents-n-ulfs.sql"
ULF_SQL_OUTPUT_FILE=${WORKING_DIR}"sid-sent-ulf-out.tsv"
FINAL_ULF_OUTPUT_FILE="usid-isid-sent-ulf-out.tsv"
TEMP_FINAL_ULF_FILE=${WORKING_DIR}${FINAL_ULF_OUTPUT_FILE}

# TODO: double check these....
INF_SQL_REQUEST_FILE=${WORKING_DIR}"request-sid-infs.sql"
INF_SQL_OUTPUT_FILE=${WORKING_DIR}"sid-sent-ulf-out.tsv"
FINAL_INF_OUTPUT_FILE="usid-isid-sent-ulf-out.tsv"
TEMP_FINAL_INF_FILE=${WORKING_DIR}${FINAL_INF_OUTPUT_FILE}

python make-sql-requests.py ${DEFAULT_ULF_ANN_FILE} ${DEFAULT_INF_ANN_FILE} ${ULF_SQL_REQUEST_FILE} ${INF_SQL_REQUEST_FILE}
mysql -h ebdb-instance2.cuainl7oscs9.us-east-2.rds.amazonaws.com -P 3306 -u gkim21 -p ebdb --password < ${ULF_SQL_REQUEST_FILE} > ${ULF_SQL_OUTPUT_FILE}
python combine-and-format-ulf-anns.py ${ULF_SQL_OUTPUT_FILE} ${DATASET_JSON_FILE} ${TEMP_FINAL_ULF_FILE}

cp ${TEMP_FINAL_ULF_FILE} ${FINAL_ULF_OUTPUT_FILE}

