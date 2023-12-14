#!/usr/bin/env bash

#### Preprocessing
if test -f /tmp/temp.txt; then
  rm /tmp/temp.txt
fi
if test -f /tmp/structDef.txt; then
  rm /tmp/structDef.txt
fi

file=$1  #get filename from command line
module=${file%.*} #module name is the filename without extension
module=$(basename $module)

#Extract lines with user defined struct definition
while read -r line; do
    if [[ $line == *"tag-struct"* ]]; then
        read -r line
      if [[ $line == *"Module"* && $line == *$module* ]]; then 
        read -r line
        read -r line
        read -r line

        line=${line//\*/"CAP$"}
        prefix=${line%%"struct"*}
        index=${#prefix}
        if [[ index -eq ${#line} ]]; then 
          echo "No"
        else        
             i=`expr $index + 1`
             echo $line | awk -v i="$index" '{print substr($0, i)}' >> /tmp/temp.txt
        fi
      fi
   fi
done <$file

#Format user defined struct definition
if test -f /tmp/temp.txt; 
then
    while read -r line; do
        echo "__STRUCT__" >> /tmp/structDef.txt
        echo $line | awk -F{ '{print $1}' >> /tmp/structDef.txt
    #    echo "__FIELDS__" >> /tmp/structDef.txt
        echo $line | grep -o -P '(?<={).*(?=})' | grep -o -P '(?<= ).*?(?=;)' >> /tmp/structDef.txt
        echo "__ENDSTRUCT__" >> /tmp/structDef.txt
    done < /tmp/temp.txt
else
    touch /tmp/structDef.txt
fi

#post processing
rm -f /tmp/temp.txt
