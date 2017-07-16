usage="
Program to score EEG matrices where higher scores represent a higher
likelihood of seizure in the immediate future. 

Usage:

  $(basename "$0") [-h] -i -o 

Arguments:
  -i  single input directory (may contain subdirectories) containing matrices
  -o  output csv
"

if [[ $# -eq 0 ]] ; then
  echo "$usage" 
  exit 0
fi

while getopts ":i:o:" option; do
  case "$option" in
    h) echo "$usage"
       exit 0
       ;;
    i) INPUT="$OPTARG" ;;
    o) OUTPUT="$OPTARG" ;;
    :) echo "missing argument" >&2; 
       echo "$usage"
       exit 1
       ;;
    \?) echo "unknown option: -$OPTARG" >&2;
       echo "$usage"
       exit 1
       ;;
    *) echo "unimplemented option: -$OPTARG" >&2;
      exit 1;;
  esac
done 

echo "Starting PyWavelets data processing in Python"
python step1_processing.py -i $INPUT -o processed_data.csv

echo "Starting model scoring in R"
Rscript --vanilla step2_scoring.R processed_data.csv $OUTPUT 

rm processed_data.csv
rm input_filenames.csv
