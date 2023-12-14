#!/bin/zsh

GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m'

npass=0
nfail=0
for test_file in test/*.c; do
  if [ -f "$test_file" ]; then
    echo "Running test case: $test_file"
    esy x instantiation exec --logging=disabled "$test_file"
    if [ $? -eq 0 ]; then
      echo "${GREEN}Test case passed: $test_file${NC}"
      ((npass++))
    else
      echo "${RED}Test case failed: $test_file${NC}"
      ((nfail++))
    fi
  fi
done 

echo "${GREEN}$npass tests passed${NC}, ${RED}$nfail tests failed${NC}"
